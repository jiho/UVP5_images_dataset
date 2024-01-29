#
# Read the final data set and make some plots
# Plots will illustrate https://docs.google.com/document/d/1nUb19jq9p106nR3EphhlgrMRpp5PhvGQeblGGSIOBRo/
#
# (c) 2021 Jean-Olivier Irisson, 2022 Julie Coustenoble, GNU General Public License v3

source("0.setup.R")
library("tidyverse")
library("patchwork")

# define colour scales
tab_colors <- c("#1F77B5", "#FF800E", "#2CA12D", "#D72727", "#9567BE", "#8E554A", "#E477C4", "#7F7F7F", "#BDBE21", "#16C0D0")
new_tab_colors <- c("#4D79A8", "#F38F2C", "#E25758", "#76B9B3", "#59A34E", "#EECA48", "#B07AA2", "#FF9FA9", "#9D745F", "#BBB1AC")

# ESSD:
# width:  single column = 8.5cm; double column = 17.7 cm
# height: total page = 23 cm
# multiplier for ggplot = 1.5
w <- 8.5   * 1.5
w2 <- 17.7 * 1.5
h <- 23    * 1.5

theme_set(theme_gray(10))

dir.create("plots", showWarnings=FALSE)

## Read data ----

# coastline
coast <- read_csv("data/gshhg_world_l.csv.gz", col_types=cols())

# sample level
smp <- read_tsv("data/final/samples.tsv.gz")
vol <- read_tsv("data/final/samples_volume.tsv.gz")

# objects level (only the relevant variables)
obj <- read_tsv("data/final/objects.tsv.gz")


## Fig 1: Table of images of consistent categories ----

# extract representative images
rep_imgs <- obj %>%
  group_by(group) %>%
  do({
    x <- .

    # limit number of objects (for not_plankton)
    lim <- 10^5
    if (nrow(x) > lim) { x <- sample_n(x, lim) }

    # avoid images too wide which would distort the table
    x <- filter(x, width < 1000)

    # perform a PCA, based on a few variables, to organise the images
    sp <- FactoMineR::PCA(select(x, area_mm2, major_mm, mean, median, fractal, perimmajor, elongation, `circ.`), graph=FALSE)

    # get representative images along the first PC
    library("FNN")
    coords <- sp$ind$coord[,1:4] %>% as_tibble()
    qPC1 <- quantile(coords$Dim.1, probs=c(0.15, 0.85))
    qPC2 <- quantile(coords$Dim.2, probs=c(0.15, 0.85))
    query <- tibble(Dim.1=c(0, qPC1[1], qPC1[2], 0, 0), Dim.2=c(0, 0, 0, qPC2[1], qPC2[2]), Dim.3=0, Dim.4=0)
    nn <- get.knnx(coords, query, k=1)

    # extract the path to the image
    x[nn$nn.index,] %>%
      transmute(
        # compute image path
        img_path=file.path(img_dir, sample_id, str_c(object_id, ".jpg")),
        # add group
        group=x$group[1]
      )
  }) %>%
  ungroup()

# plot them
library("gt")
library("gtExtras")
pic_tbl <- rep_imgs %>%
  mutate(n=rep(1:5, times=length(unique(group)))) %>%
  select(group, n, img_path) %>%
  pivot_wider(names_from=n, values_from=img_path) %>%
  gt() %>%
  gt_img_rows(columns=`1`, img_source="local", height = 100) %>%
  gt_img_rows(columns=`2`, img_source="local", height = 100) %>%
  gt_img_rows(columns=`3`, img_source="local", height = 100) %>%
  gt_img_rows(columns=`4`, img_source="local", height = 100) %>%
  gt_img_rows(columns=`5`, img_source="local", height = 100) %>%
  tab_options(column_labels.hidden=TRUE)
pic_tbl
gtsave(pic_tbl, "plots/example_images.html")


## Fig 2: Spatio-temporal extent ----

# Piece 1: Map
(p_map <- ggplot(mapping=aes(lon, lat)) +
  # coastline
  # NB: coord_map does weird things to the polar polygons...
  coord_quickmap() +
  geom_polygon(data=coast, fill="grey99") +
  # profiles
  geom_point(aes(shape=uvp_model, colour = uvp_model), data=smp, size=1, alpha=0.5) +
  #scale_shape_manual("", values=c(4,1,17)) +
  # nice breaks and labels
  scale_x_continuous(
    minor_breaks=NULL, expand=c(0,0),
    labels=function(x) {paste0(abs(x), "°", c("W", "", "E")[sign(x) + 2])}
  ) +
  scale_y_continuous(
    breaks=c(-60, -30, 0, 30, 60), minor_breaks=NULL, expand=c(0,0),
    labels=function(x) {paste0(abs(x), "°", c("S", "", "N")[sign(x) + 2])}
  ) +
  scale_color_manual(values=tab_colors[c(2,1,3)]) +
  # shape legend
  theme(
    legend.position=c(0.72,0.79),
    legend.direction="horizontal",
    legend.background=element_blank(),
    legend.title=element_blank()
  ) +
  guides(shape=guide_legend(override.aes=list(size=2, alpha=1))) +
  # remove lat/lon titles
  theme (axis.title=element_blank())
)

# Piece 2: time-line
# with some latitude resolution
(p_ts <- ggplot(smp) +
  geom_point(aes(x=datetime, y=cut(lat, c(-90, -30, 30, 90), labels=c("]90°S,30°S]", "]30°S,30°N]", "]30°N,90°N]")), colour=uvp_model),
             shape="|", alpha=0.1, size=4)+
  scale_color_manual(values=tab_colors[c(2,1,3)], guide="none") +
  labs(x="Date of profile", y="Latitude"))


# Piece 3: Depth distribution
# compute maximum depth per profile and add it to the samples table
smp <- left_join(
  x = smp,
  y = vol %>%
    group_by(sample_id) %>%
    summarise(max_depth=max(mid_depth_bin)) %>%
    ungroup(),
  by ="sample_id"
)

# sqrt transform for negative values
# (copied and modified from sqrt_trans in scales/R/trans-numeric.r)
neg_sqrt_trans <- function() {
  scales::trans_new("sqrt",
                    function(x) {-sqrt(-x)},
                    function(x) -(x^2),
                    domain = c(0, Inf))
}

(p_depth <- ggplot(smp) +
  geom_histogram(aes(y=-max_depth, fill=uvp_model), binwidth=1) +
  scale_y_continuous(
    # nice breaks
    trans="neg_sqrt",
    breaks=-c(1, 500, 1000, 2000, 4000, 6000),
    # put the legend on the right for better composition
    position="right"
  ) +
  scale_fill_manual(values=tab_colors[c(2,1,3)], guide="none") +
  labs(x="Nb of profiles", y="Maximum depth [m]"))

# Put everything together

# layout the plot
layout <- "
AAAAB
AAAAB
AAAAB
CCCCB
"
p_map + p_depth + p_ts + plot_layout(design=layout)

# and save
ggsave(file="plots/map_timeline_depth.pdf", width=w2, height=8.18*1.5, unit="cm")


## Fig 3: Barplot of total number of images per taxon ----

taxa_counts <- count(obj, group) %>%
  arrange(desc(n))
taxa_counts %>%
  mutate(group=factor(group, levels=group)) %>%
  ggplot() +
  geom_bar(aes(x=group, y=n), stat="identity") +
  scale_y_continuous(trans="sqrt") +
  labs(y="Nb of objects in data set", x=NULL) +
  theme(axis.text.x=element_text(angle=45, hjust=1))
ggsave("plots/number_per_taxon.pdf", width=w2, height=w*0.8, unit="cm")


## Fig 4: Concentration for the most abundant taxa along depth ----

# define abundant taxa
abundant_taxa <- taxa_counts %>%
  filter(!str_detect(group, "_")) %>%
  head(5) %>%
  pluck("group")

# compute concentrations, biovolume and grey level per profile
source("data/final/compute_properties_per_bin.R")
props <- obj %>%
  filter(group %in% c("not_plankton", abundant_taxa)) %>%
  properties_per_bin(vol, depth_breaks=c(0, 100, 500, 1000, 2000, 4000)) %>%
  mutate(depth_bin=ordered(depth_bin))

# average across all profiles
props_sum <- props %>%
  group_by(depth_bin, group) %>%
  summarise(
    conc=mean(concentration),
    biovol=mean(biovolume),
    avg_grey=mean(avg_grey, na.rm=TRUE),
    .groups="drop") %>%
  mutate(
    living=ifelse(group=="not_plankton", "Not plankton (marine snow, ...)", "Plankton"),
    group=as.character(group),
    group=ifelse(group=="not_plankton", NA, group),
    depth_bin=factor(depth_bin, levels=rev(unique(depth_bin)))
  )

# plot each variable
bar_conc <- props_sum %>%
  ggplot() +
    facet_wrap(~living, ncol=1, scales="free_x", strip.position="right") +
    geom_bar(aes(y=depth_bin, x=conc, fill=group), stat="identity") +
    labs(x="Concentration (#/L)", y="Depth", fill="Taxon") +
    scale_fill_brewer(palette="Set1", na.value="grey40")
bar_biov <- props_sum %>%
  ggplot() +
  facet_wrap(~living, ncol=1, scales="free_x", strip.position="right") +
  geom_bar(aes(y=depth_bin, x=biovol, fill=group), stat="identity") +
  labs(x="Biovolume (mm3/L)", y="Depth", fill="Taxon") +
  scale_fill_brewer(palette="Set1", na.value="grey40") +
  theme(axis.text.y=element_blank(), axis.title.y=element_blank(), axis.ticks.y=element_blank())
path_grey <- props_sum %>%
  ggplot() +
  facet_wrap(~living, ncol=1, scales="free_x", strip.position="right") +
  geom_path(aes(y=depth_bin, x=avg_grey, colour=group, group=group)) +
  geom_point(aes(y=depth_bin, x=avg_grey, colour=group)) +
  scale_x_continuous(limits=c(181,229)) +
  labs(x="Average grey level (~ opacity)", y="Depth", fill="Taxon") +
  scale_colour_brewer(palette="Set1", na.value="grey40", guide="none")+
  theme(axis.text.y=element_blank(), axis.title.y=element_blank(), axis.ticks.y=element_blank())

# combine the plots
bar_conc + bar_biov + path_grey + plot_layout(guides = "collect") & theme(legend.position="bottom")
ggsave("plots/conc_biovol_grey_with_depth.pdf", width=w2, height=w*1.2, unit="cm")


## Fig 5: Size spectrum ----
# Histogram/spectrum of the size of the ~five most abundant taxa + detritus separating 0:200m and 200m:bottom

# compute volume sampled in each depth bin
vol_per_bin <- vol %>%
  mutate(depth_bin=cut(mid_depth_bin, breaks=c(0,100,500,1000), include.lowest=TRUE, dig.lab=5)) %>%
  group_by(sample_id, depth_bin) %>%
  summarise(water_volume_imaged=sum(water_volume_imaged), .groups="drop")

obj_s <- obj %>%
  select(sample_id, object_id, depth, group, esd_mm, vol_mm3) %>%
  # keep only abundant taxa
  filter(group %in% c(abundant_taxa, "not_plankton")) %>%
  # add UVP model and keep only HD and SD
  left_join(select(smp, sample_id, uvp_model)) %>%
  filter(uvp_model!="ZD") %>%
  # separate in various depth bins
  mutate(depth_bin=cut(depth, breaks=c(0,100,500,1000), include.lowest=TRUE, dig.lab=5)) %>%
  drop_na(depth_bin) %>%
  # compute "unitary" concentrations (to serve as weights in the NBSS computation)
  left_join(vol_per_bin) %>%
  mutate(conc=1/water_volume_imaged)

# check
sum(is.na(obj_s$conc))
# -> OK

# compute NBSS per taxon, depth bin and profile
library("nbssr")
NBSS <- obj_s %>%
  group_by(sample_id, group, depth_bin, uvp_model) %>%
  do({
    nbss(.$vol_mm3, w=.$conc, binwidth=0.2)
  }) %>%
  ungroup()

ggplot(NBSS) +
  facet_grid(depth_bin~group) +
  geom_path(aes(x=bin, y=norm_y, group=sample_id), alpha=0.01) +
  scale_x_log10(limits=c(0.1, 10000)) + scale_y_log10()

### Concentration map

prep_map_data<-function(lineage){

    # 0,100 / 100,200 / 200,1000 / 1000,6000
  layers=list(list(0,100), list(100,200), list(200,1000), list(1000,6000))

  final_map_data=	data.frame()

  for(layer in layers){
    start_layer = layer[1]
    end_layer = layer[2]
    data <- unique(obj[c("profile_id", "object_id", "depth", "group", "group_lineage")]) %>%
      filter(between(depth, start_layer, end_layer)) %>%
      filter(startsWith(group_lineage, lineage)|startsWith(group, lineage)) %>%
      merge(
        x = .,
        y = unique(smp[c("profile_id", "lat", "lon" )]),
        by = "profile_id",
        all.x = TRUE
      ) %>%
      group_by(profile_id)%>% add_tally()
      final_map_data_layer <- vol %>%
        filter(between(mid_depth_bin, start_layer, end_layer)) %>%
        group_by(profile_id)%>%
        summarise(tot_vol_m3 = sum(water_volume_imaged)/1000)%>%
        merge(
          x = .,
          y = unique(data[c("profile_id", "lon", "lat", "n")]),
          by = "profile_id",
          all = TRUE
        ) %>%
        mutate(concentration = n / tot_vol_m3)%>%
        mutate(layer = paste(start_layer, "-",end_layer,"m"))

      final_map_data_layer<-na.omit(final_map_data_layer)
      final_map_data<-rbind(final_map_data,final_map_data_layer)
  }

  return(final_map_data)
}

concentration_map <- function(final_map_data, lineage) {

  coast <- read_csv(paste0("data/gshhg_world_c.csv", col_types=cols()))
  plot_map <-
    ggplot(final_map_data, fill=layer) +
    geom_polygon(aes(lon, lat), data=coast, fill="grey75") +
    geom_point(aes(lon, lat, size=concentration), shape=1, alpha=0.5) +
    coord_quickmap() + scale_xy_map() +
    labs(title=lineage,size = "Concentration\n(ind/m3)")+ #1 10 100 1000 10000 OU 100 1000 10000 100000
    scale_size_continuous(trans="sqrt", range = c(0.1, 6), breaks=c(1,10,100,1000,10000,100000))+
    facet_wrap(~factor(layer, levels=c(unique(final_map_data$layer))), nrow = 4)
  return(plot_map)
}

# Get data
map_data_liv=prep_map_data('living')
map_data_det=prep_map_data('detritus')
# Create maps
map_living<-concentration_map(map_data_liv, 'Total plankton concentration in 4 depth layers\n')
map_detritus<-concentration_map(map_data_det, 'Total detritus concentration in 4 depth layers\n')

layout_maps <- "
AABB
AABB
AABB
AABB
"

map_living + map_detritus + plot_layout(design=layout_maps)

# and save
ggsave(file="data/final/plot_maps_sqrt.pdf", width=40, height=30, unit="cm")



#### Bar plot concentration per taxon
prep_profiles_for_pertinent_categories_data<-function(){
  # select only lineages taxa
  #"not-living/detritus", "Trichodesmium", "Phaeodaria", "Copepoda"
  #groups = list("Trichodesmium")
  groups = unique(  obj %>%
                      filter(startsWith(group_lineage, 'living') | startsWith(group_lineage, 'not-living/detritus') ) %>%
                      .$group)

  # add depth label
  # "0 - 100 m", "100 - 200 m", "200 - 1000 m", "> 1000 m
  # 0,100 / 100,200 / 200,1000 / 1000,6000
  layers=list( list(0,100), list(100,200), list(200,1000),list(1000,6000))

  merged_data <- unique(obj[c("profile_id", "object_id", "depth", "group", "group_lineage")])%>%
    filter(startsWith(group_lineage, 'living') | startsWith(group_lineage, 'not-living/detritus') )  %>%
    merge(
      x = .,
      y = unique(smp[c("profile_id", "lat" )]),
    )

  #### cons_data
  cons_data <- data.frame()

  for(layer in layers){

    data_layer <- data.frame()

    start_layer = layer[1]
    end_layer = layer[2]

    vol_layer <- vol %>%
      filter(between(mid_depth_bin, start_layer, end_layer)) %>%
      group_by(profile_id)%>%
      summarise(tot_vol_m3 = sum(water_volume_imaged)/1000)

    for(taxo in groups){
      data_concentration<- merged_data %>%
        filter(between(depth, start_layer, end_layer)) %>%
        filter(startsWith(group, taxo)) %>%
        merge(x=.,
              y=vol_layer,
              all.y = TRUE
        ) %>%
        group_by(profile_id, tot_vol_m3)%>%
        count(group)%>%
        mutate(concentration = n / tot_vol_m3)%>%
        subset(., select=c("concentration"))%>%
        summarise(concentration_median = quantile(concentration, 0.5)) %>%
        #mutate(concentration_median = mean(concentration)) %>%
        mutate(taxo = taxo) %>%
        mutate(layer = paste(start_layer, "-",end_layer,"m"))

      # set to 0 missing concentration data
      data_concentration[is.na(data_concentration)] <- 0
      print(data_concentration)
      data_layer<-rbind(data_layer,data_concentration)
    }
    data_layer<-data_layer%>%
      arrange(desc(concentration_median)) %>%
      head(15)
    cons_data<-rbind(cons_data,data_layer)
  }
  return(cons_data)
}

plot_concentration_per_taxon_data<- function(df) {
  # reorder groups
  df <- as.data.table(df)[, concentration_median, by = .(layer,taxo)]
  # create var which reflects order when sorted alphabetically
  df[, ord := sprintf("%02i", frank(df, concentration_median, ties.method = "first"))]

  # plot
  p_concentration_by_taxon_living<-
    ggplot(df %>%
             filter(!startsWith(taxo, 'detritus')) ,
           aes(y=ord, x=concentration_median))+
    geom_bar(stat="identity")+
    ggtitle("Plankton concentration in 4 depth layers")+
    xlab("Concentration (ind/m3)")+
    ylab("Taxa")+
    scale_y_discrete(labels = df[, setNames(as.character(taxo), ord)]) +
    facet_wrap(~factor(layer, levels=c(unique(df$layer))), nrow = 1, scales="free", drop = TRUE)

  p_concentration_by_taxon_detritus<-
    ggplot(df %>%
             filter(startsWith(taxo, 'detritus')),
           aes(y=ord, x=concentration_median))+
    geom_bar(stat="identity")+
    ggtitle("Detritus concentration in 4 depth layers")+
    xlab(NULL)+
    ylab(NULL)+
    scale_y_discrete(labels = df[, setNames(as.character(taxo), ord)]) +
    facet_wrap(~factor(layer, levels=c(unique(df$layer))), nrow = 1, scales="free_y", drop = TRUE)


  return(list(p_concentration_by_taxon_living=p_concentration_by_taxon_living, p_concentration_by_taxon_detritus=p_concentration_by_taxon_detritus))
}



concentration_per_taxon_data = prep_profiles_for_pertinent_categories_data()

ret = plot_concentration_per_taxon_data(concentration_per_taxon_data)
p_concentration_by_taxon_living <- ret$p_concentration_by_taxon_living
p_concentration_by_taxon_detritus <- ret$p_concentration_by_taxon_detritus


# over some depth bins
# layout the plot
layout_concentration_bar <- "
BBBB
AAAA
AAAA
AAAA
AAAA
AAAA
AAAA
AAAA
AAAA
"
p_concentration_by_taxon_living + p_concentration_by_taxon_detritus + plot_layout(design=layout_concentration_bar)

# and save
ggsave(file="data/final/plot_concentration_bar.pdf", width=30*1.5, height=19*1.5, unit="cm")


##### Average size distribution of SD and HD versions

######## TEST DATA ########
######## TEST DATA ########

final_data <- unique(obj[c("profile_id", "object_id",  "depth", "esd_mm", "group_lineage")]) %>%
  filter(depth<=100)  %>%
  merge(
    x = .,
    y = unique(smp[c("profile_id","uvp_model", "project")]),
    by = "profile_id",
    all.x = TRUE)%>%
  filter(str_detect(project, 'ccelter')|str_detect(project, '_lter'))%>%
  filter(!startsWith(uvp_model, "ZD"))

##LIVING
living <- final_data%>% filter(startsWith(group_lineage, "living/"))
min(living %>% filter(startsWith(uvp_model, "HD"))%>%.$esd_mm) #[1] 0.8733655
min(living %>% filter(startsWith(uvp_model, "SD"))%>%.$esd_mm) #[1] 0.8771137
max(living %>% filter(startsWith(uvp_model, "HD"))%>%.$esd_mm) #[1] 48.59652
max(living %>% filter(startsWith(uvp_model, "SD"))%>%.$esd_mm) #[1] 60.27


##DETRITUS
detritus <- final_data%>% filter(startsWith(group_lineage, "not-living/detritus"))
min(detritus %>% filter(startsWith(uvp_model, "HD"))%>%.$esd_mm) #[1] 0.8733655
min(detritus %>% filter(startsWith(uvp_model, "SD"))%>%.$esd_mm) #[1] 0.6419815
max(detritus %>% filter(startsWith(uvp_model, "HD"))%>%.$esd_mm) #[1] 17.71171
max(detritus %>% filter(startsWith(uvp_model, "SD"))%>%.$esd_mm) #[1] 40.45944

######## TEST DATA ########
######## TEST DATA ########

prep_size_distribution_per_version_data<-function(model){
  # SD or HD
  #versions=list("SD","HD")
  tot_vol_0_100_m3=vol %>%
    filter(mid_depth_bin<=200)%>%
    merge(
      x = .,
      y = unique(smp[c("profile_id", "lat", "lon", "uvp_model", "project")]%>%
                   filter(startsWith(uvp_model, model))),
      #%>%filter(stringr::str_detect(project, 'tara') ) ),
      by = "profile_id",
      all.y = TRUE)%>%
    group_by(profile_id)%>%
    summarise(tot_vol_m3 = sum(water_volume_imaged)/1000)

  #filter(startsWith(group_lineage, 'living') |
  final_data <- unique(obj[c("profile_id", "object_id", "depth", "esd_mm", "group", "group_lineage")]) %>%
    filter(depth<=200)%>%
    merge(
      x = .,
      y = unique(smp[c("profile_id","uvp_model", "lat", "lon", "project")]%>%
                   filter(startsWith(uvp_model, model))),
      #%>%filter(stringr::str_detect(project, 'tara') ) ),
      by = "profile_id",
      all.y = TRUE)%>%
    mutate(cat=case_when(startsWith(group_lineage,"living") ~ 'Living',
                         startsWith(group_lineage,"not-living/detritus") ~ 'Detritus',
                         startsWith(group_lineage,"not-living/artefact") ~ 'Artefact') ) %>%
    #startsWith(group_lineage,"not-living/detritus") ~ 'Detritus')) %>%
    drop_na()

  # mutate(cat=case_when(startsWith(group_lineage,"living") ~ 'Living')) %>%
  #startsWith(group_lineage,"not-living/detritus") ~ 'Detritus')) %>%

  nbss_data = final_data
  tot_vol = tot_vol_0_100_m3
  # Create a table to keep all the individual ss table (1 table = 1 profile)
  ss_all <- matrix(ncol = 10,
                   dimnames=list(c(), c(
                     "profile_id", "bin_log","bin","binwidth",
                     "y","norm_y", "norm_y_vol", "bin_min","bin_max", "cat")))
  ss_all=data.frame(ss_all)


  # for each profile, do the ss table and divide by the volume sampled
  for (i in 1:nrow(tot_vol)) {

    # get the organisms from the profile i
    nbss_profile <- nbss_data %>%  filter(profile_id == tot_vol$profile_id[[i]])
    for(categ in c("Living", "Detritus", "Artefact")){
      # If the dataset isn't empty, continue
      nbss_profile_categ <- nbss_profile %>%
        filter(startsWith(cat, categ))

      if (plyr::empty(nbss_profile_categ) != TRUE) {
        # compute the size spectrum
        ss <- nbss_function(nbss_profile_categ$esd_mm, type="abundance", binwidth=0.05)
        # divide the normalised number norm_y (= y / binwidth) by the sampled volume
        ss$norm_y_vol <- ss$norm_y / tot_vol$tot_vol_m3[[i]]
        # add the profile_id number in the table
        ss$profile_id <- tot_vol$profile_id[[i]]
        #add categ
        ss$cat<-categ
        # add it to the ss_all dataframe
        ss_all <- rbind(ss_all, ss)
      } else{
        ss <- data.frame(matrix(ncol = 10,
                                dimnames=list(c(), c(
                                  "profile_id", "bin_log","bin","binwidth",
                                  "y","norm_y", "norm_y_vol", "bin_min", "bin_max", "cat"))))
        ss$profile_id <- tot_vol$profile_id[[i]]
        ss$cat<-categ
        # add it to the ss_all dataframe
        ss_all <- rbind(ss_all, ss) %>%
          drop_na()
      }
    }
  }
  # remove values we don't need
  ss_all <- ss_all %>%
    filter(!is.na(bin_log)) %>%
    filter_all(all_vars(!is.infinite(.))) %>%
    filter(norm_y_vol > 0)

  ss_all <- ss_all %>%
    merge(
      x = .,
      y = unique(final_data[c("profile_id","cat", "lat", "lon" )]),
      all.x = TRUE)
  #browser()

  # compute the mean of each bin
  ss_mean <- ss_all %>%  group_by(bin, cat) %>%
    summarise(mean_bin = mean(norm_y_vol))

  return(list(ss_all=ss_all, ss_mean=ss_mean))
}


plot_size_distribution_per_version_data<- function(data, group) {
  ss_all<-data$ss_all
  ss_mean<-data$ss_mean
  # Plot on point for each profile
  plot_all <- ggplot() +
    geom_point(data = ss_all, aes(x=bin, y=norm_y_vol, colour = cat), alpha = 0.2) +
    geom_smooth(data = ss_mean, aes(x=bin, y=mean_bin, colour = cat)) +
    # You can use geom_point below to show the mean for each bin
    #geom_point(data = ss_mean, aes(x=bin, y=mean_bin, color = cat)) +
    # You can use geom_line to connect these mean values
    #geom_line(data = ss_mean, aes(x=bin, y=mean_bin, color = cat)) +
    #scale_shape_manual("Status", values=c(19, 4)) +
    #xlim(c(0,100)) +
    # log transformation
    scale_x_log10() + scale_y_log10() +
    # change name of axes
    labs(y="Normalised abundance (nb m-3 mm-1)",
         x = "ESD (mm)")+
    # use a title according to the latitudinal band
    ggtitle(paste("Average size distribution of Living and Detritus for ", group))

  plot_smooth <- ggplot() +
    #geom_point(data = ss_all, aes(x=bin, y=norm_y_vol, colour = cat), alpha = 0.2) +
    geom_smooth(data = ss_mean, aes(x=bin, y=mean_bin, colour = cat)) +
    # log transformation
    scale_x_log10() + scale_y_log10() +
    # change name of axes
    labs(y="Normalised abundance (nb m-3 mm-1)",
         x = "ESD (mm)")+
    # use a title according to the latitudinal band
    ggtitle(paste("Average size distribution of Living and Detritus for ", group))

  return(list(plot_all=plot_all,plot_smooth=plot_smooth))
}

size_distribution_per_version_data_HD = prep_size_distribution_per_version_data("HD")
p_size_distribution_per_version_data_HD = plot_size_distribution_per_version_data(size_distribution_per_version_data_HD,"HD")

size_distribution_per_version_data_SD = prep_size_distribution_per_version_data("SD")
p_size_distribution_per_version_data_SD = plot_size_distribution_per_version_data(size_distribution_per_version_data_SD, "SD")

# over some depth bins
# layout the plot
layout_plot_average_size_distribution <- "
AABB
CCDD
"
p_size_distribution_per_version_data_SD$plot_all + p_size_distribution_per_version_data_HD$plot_all + p_size_distribution_per_version_data_SD$plot_smooth + p_size_distribution_per_version_data_HD$plot_smooth + plot_layout(design=layout_plot_average_size_distribution)

# and save
ggsave(file="data/final/plot_average_size_distribution.pdf", width=30, height=20, unit="cm")


#### Vertical profiles of detritus and plankton concentrations/ESD at different latitude

prep_profiles_for_pertinent_categories_data<-function(){
  # select only lineages taxa
  #"not-living/detritus", "Trichodesmium", "Phaeodaria", "Copepoda"
  #groups = list("Trichodesmium")
  groups = list("detritus", "Copepoda", "Phaeodaria", "Trichodesmium")

  #add lat label
  #"0°30°" , "30°60°", "60°90°"
  regions=list(list(0,30), list(30,60), list(60,90))

  # add depth label
  # "0 - 100 m", "100 - 200 m", "200 - 1000 m", "> 1000 m
  # 0,100 / 100,200 / 200,1000 / 1000,6000
  layers=list(list(1000,6000), list(200,1000), list(100,200), list(0,100))

  merged_data <- unique(obj[c("profile_id", "object_id", "depth", "esd_mm", "group", "group_lineage")]) %>%
    merge(
      x = .,
      y = unique(smp[c("profile_id", "lat" )]),
    )

  #### esd_data
  esd_data <- data.frame()
  #### cons_data
  cons_data <- data.frame()

  for(taxo in groups){
    #print(taxo)
    for(region in regions){
      start_lat = region[1]
      end_lat = region[2]
      #print(start_lat)
      for(layer in layers){
        start_layer = layer[1]
        end_layer = layer[2]
        #print(start_layer)

        data_size <- merged_data %>%
          filter(startsWith(group, taxo)) %>%
          filter(between(lat, start_lat, end_lat)) %>%
          filter(between(depth, start_layer, end_layer)) %>%
          summarise(esd_mm_median = quantile(esd_mm, 0.5), esd_mm_q1 = quantile(esd_mm, 0.25), esd_mm_q3 = quantile(esd_mm, 0.75)) %>%
          mutate(taxo = taxo) %>%
          mutate(region = paste(start_lat, "°",end_lat,"°"))%>%
          mutate(layer = paste(start_layer, "-",end_layer,"m"))

        # drop missing size data
        data_size<-na.omit(data_size)
        esd_data<-rbind(esd_data,data_size)

        data_concentration<- merged_data %>%
          filter(startsWith(group, taxo)) %>%
          filter(between(lat, start_lat, end_lat))%>%
          filter(between(depth, start_layer, end_layer)) %>%
          merge(x=.,
                y=vol %>%
                  filter(between(mid_depth_bin, start_layer, end_layer)) %>%
                  group_by(profile_id)%>%
                  summarise(tot_vol_m3 = sum(water_volume_imaged)/1000)
          ) %>%
          group_by(profile_id, tot_vol_m3)%>%
          count(group)%>%
          mutate(concentration = n / tot_vol_m3)%>%
          subset(., select=c("concentration"))%>%
          summarise(concentration_median = quantile(concentration, 0.5), concentration_q1 = quantile(concentration, 0.25), concentration_q3 = quantile(concentration, 0.75)) %>%
          mutate(taxo = taxo) %>%
          mutate(region = paste(start_lat, "°",end_lat,"°"))%>%
          mutate(layer = paste(start_layer, "-",end_layer,"m"))

        # set to 0 missing concentration data
        data_concentration[is.na(data_concentration)] <- 0
        cons_data<-rbind(cons_data,data_concentration)

      }
    }
  }

  return(list(cons_data=cons_data, esd_data=esd_data))
}

plot_profiles_for_pertinent_categories_data<- function(data) {

  esd_data<-data$esd_data
  cons_data<-data$cons_data

  # Plot on point for esd
  plot_esd_data <- ggplot(data = esd_data, aes(x=esd_mm_median, y=layer)) +
    geom_point(aes(group=region, colour=region)) +
    geom_line(aes(group=region, colour=region),orientation = "y") +
    geom_ribbon(aes(group=region, y=layer, xmin = esd_mm_q1, xmax = esd_mm_q3, fill = region), alpha=0.1) +
    labs(y="Depth (m)",
         x = "ESD (mm)")+
    scale_y_discrete(limits = c(unique(esd_data$layer)))+
    ggtitle("Vertical profiles of detritus and plankton average size (ESD) at different latitude") +
    facet_wrap(~factor(taxo, levels=c(unique(esd_data$taxo))), nrow = 2, drop = TRUE)

  # Plot on point for abd
  plot_cons_data <- ggplot(data = cons_data, aes(x=concentration_median+1 , y=layer)) +
    geom_point(aes(group=region, colour=region)) +
    geom_line(aes(group=region, colour=region),orientation = "y") +
    geom_ribbon(aes(group=region, y=layer, xmin = concentration_q1+1, xmax = concentration_q3+1, fill = region), alpha=0.1) +
    labs(y="Depth (m)",
         x = "Concentration (ind/m3 -1)")+
    scale_y_discrete(limits = c(unique(cons_data$layer)))+
    scale_x_log10() +
    ggtitle("Vertical profiles of detritus and plankton concentrations at different latitude") +
    facet_wrap(~factor(taxo, levels=c(unique(cons_data$taxo))), nrow = 2, drop = TRUE)

  return(list(plot_esd_data=plot_esd_data, plot_cons_data=plot_cons_data))
}

profiles_for_pertinent_categories_data = prep_profiles_for_pertinent_categories_data()

p_profiles_for_pertinent_categories_data = plot_profiles_for_pertinent_categories_data(profiles_for_pertinent_categories_data)
plot_esd_data<-p_profiles_for_pertinent_categories_data$plot_esd_data
plot_cons_data<-p_profiles_for_pertinent_categories_data$plot_cons_data


# over some depth bins
# layout the plot
layout_plot_vertical_profiles <- "
AAAA
AAAA
AAAA
AAAA
BBBB
BBBB
BBBB
BBBB
"
plot_esd_data + plot_cons_data  + plot_layout(design=layout_plot_vertical_profiles)

# and save
ggsave(file="data/final/plot_vertical_profiles.pdf", width=30, height=40, unit="cm")

# now compute the average NBSS across all profiles
NBSSm <- NBSS %>%
  group_by(group, depth_bin, uvp_model, bin_log, bin, binwidth) %>%
  summarise(
    q25  =  quantile(norm_y, probs=0.25),
    q50  =  quantile(norm_y, probs=0.5),
    q75  =  quantile(norm_y, probs=0.75),
    mad  =  mad(norm_y),
    mean = mean(norm_y),
    sd   =  sd(norm_y),
    n = n()
    , .groups="drop"
  )

##### Geospacial informations for uvp projects
blues <- c("#81b1db", "#3971a3", "#063763")
NBSSm %>%
  filter(bin <= 10000, n>20) %>%
  ggplot() +
    facet_grid(uvp_model~group) +
    geom_ribbon(aes(x=bin, ymin=q25, ymax=q75, fill=depth_bin), alpha=0.3) +
    geom_path(aes(x=bin, y=q50, colour=depth_bin), size=0.5) +
    scale_x_log10(limits=c(0.1, 10000), sec.axis=sec_axis(trans=~2*(./pi*3/4)^(1/3) , name="Object ESD (mm)")) +
    scale_y_log10() +
    scale_colour_manual(values=blues) + scale_fill_manual(values=blues) +
    labs(x="Object volume (mm3)", y="Normalised volume (mm3/L/mm3)", colour="Depth layer", fill="Depth layer") +
    theme(panel.grid.minor=element_blank(), legend.position=c(0.95, 0.5))

# "UVP project name" , EcotaxaID, "Profiles", "Images", "Time periode", "Latitude range", "Longitude range", "UVP manager(s)"
projects <- read_csv("https://docs.google.com/spreadsheets/d/1CrR-5PdhQ09JSTU482HOkTjtCvXRmpngYDuYSwaGQAo/export?format=csv", col_types=cols())

merged_data <- unique(obj[c("profile_id", "object_id")]) %>%
  merge(
    x = .,
    y = unique(smp[c("profile_id", "project", "lat", "lon", "datetime" )]),
  )  %>%
  merge(x=.,
           y=projects[c("ptitle", "data_owner", "projid", "title")],
           by.x = "project",
           by.y = "ptitle"
  )
ggsave("plots/size_spectra.pdf", width=w2, height=w, unit="cm")

#"lat", "lon", "datetime"
uvp_project_data <- merged_data%>%
  group_by(projid)%>%
  mutate("Objects" = n_distinct(object_id))  %>%
  mutate("Profiles" = n_distinct(profile_id))  %>%
  mutate('Latitude range'=paste(min(lat)," to ", max(lat)))%>%
  mutate('Longitude range'=paste(min(lon)," to ", max(lon)))%>%
  mutate('Time period'=paste(min(datetime)," to ", max(datetime)))%>%
  ungroup()

uvp_project_data<-unique(uvp_project_data[c("title", "projid", "Profiles", "Objects", "Latitude range", "Longitude range", 'Time period', "data_owner")])
## Fig: Concentration map ----

# compute concentrations, biovolumes, etc. in consistent depth levels
props <- obj %>%
  filter(!group %in% c("bubble")) %>%
  mutate(group=ifelse(group %in% c("not_plankton"), "not_plankton", "plankton")) %>%
  properties_per_bin(vol, depth_breaks=c(0, 100, 500, 1000)) %>%
  left_join(select(smp, sample_id, lon, lat))

# read the coastline of the world
coast <- read_csv("data/gshhg_world_c.csv.gz", show_col_types=FALSE)

# plot not plankton (detritus) and plankton separately, to get different concentrations scales
map_det <- ggplot(filter(props, group=="not_plankton")) +
  geom_polygon(aes(lon, lat), data=coast, fill="white") +
  chroma::scale_xy_map() + coord_quickmap() +
  facet_grid(depth_bin~.) +
  geom_point(aes(lon, lat, size=concentration), shape=1, alpha=0.5) +
  scale_size(range=c(0.2, 10))
map_plank <- map_det %+% filter(props, group=="plankton")

# combine the two
(map_det + labs(size="Concentration\nof detritus (#/L)")) + (map_plank + labs(size="Concentration\nof plankton (#/L)"))
# and save the result
ggsave("plots/map_concentrations.pdf", width=w2, height=w, unit="cm")

write_tsv(uvp_project_data, "data/final/UVP5_project_data.tsv")
