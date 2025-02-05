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
coast <- read_csv("data/gshhg_world_l.csv.gz", show_col_types=FALSE)

# sample level
smp <- read_tsv("data/final/samples.tsv.gz", show_col_types=FALSE)
vol <- read_tsv("data/final/samples_volume.tsv.gz", show_col_types=FALSE)

# objects level (only the relevant variables)
obj <- read_tsv("data/final/objects.tsv.gz", show_col_types=FALSE)


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
    legend.position="inside",
    legend.position.inside=c(0.72,0.79),
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
  head(7) %>%
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
    n_grey=sum(!is.na(avg_grey)),
    avg_grey=mean(avg_grey, na.rm=TRUE),
    .groups="drop") %>%
  mutate(
    living=ifelse(group=="Not plankton", "Not plankton (marine snow, ...)", "Plankton"),
    group=as.character(group),
    group=ifelse(group=="Not plankton", NA, group),
    depth_bin=factor(depth_bin, levels=rev(unique(depth_bin)))
  )

# plot each variable
bar_conc <- props_sum %>%
  ggplot() +
    facet_wrap(~living, ncol=1, scales="free_x", strip.position="right") +
    geom_bar(aes(y=depth_bin, x=conc, fill=group), stat="identity") +
    labs(x="Concentration (#/L)", y="Depth", fill="Taxon") +
    # scale_fill_brewer(palette="Set1", na.value="grey40") +
    scale_fill_manual(values=new_tab_colors, na.value="grey40")
bar_biov <- props_sum %>%
  ggplot() +
  facet_wrap(~living, ncol=1, scales="free_x", strip.position="right") +
  geom_bar(aes(y=depth_bin, x=biovol, fill=group), stat="identity") +
  labs(x="Biovolume (mm3/L)", y="Depth", fill="Taxon") +
  # scale_fill_brewer(palette="Set1", na.value="grey40") +
  scale_fill_manual(values=new_tab_colors, na.value="grey40") +
  theme(axis.text.y=element_blank(), axis.title.y=element_blank(), axis.ticks.y=element_blank())
path_grey <- props_sum %>%
  filter(n_grey > 50) %>%
  ggplot() +
  facet_wrap(~living, ncol=1, scales="free_x", strip.position="right") +
  geom_path(aes(y=depth_bin, x=avg_grey, colour=group, group=group)) +
  geom_point(aes(y=depth_bin, x=avg_grey, colour=group)) +
  scale_x_continuous(limits=c(181,229)) +
  labs(x="Average grey level (~ opacity)", y="Depth", colour="Taxon") +
  # scale_colour_brewer(palette="Set1", na.value="grey40", guide="none") +
  scale_colour_manual(values=new_tab_colors, na.value="grey40", guide="none") +
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
  left_join(select(smp, sample_id, uvp_model), by="sample_id") %>%
  filter(uvp_model!="ZD") %>%
  # separate in various depth bins
  mutate(depth_bin=cut(depth, breaks=c(0,100,500,1000), include.lowest=TRUE, dig.lab=5)) %>%
  drop_na(depth_bin) %>%
  # compute "unitary" concentrations (to serve as weights in the NBSS computation)
  left_join(vol_per_bin, by=c("sample_id", "depth_bin")) %>%
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
# plot the invidual ones to check
# ggplot(NBSS) +
#   facet_grid(depth_bin~group) +
#   geom_path(aes(x=bin, y=norm_y, group=sample_id), alpha=0.01) +
#   scale_x_log10(limits=c(0.1, 10000)) + scale_y_log10()

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

blues <- c("#81b1db", "#3971a3", "#063763")
NBSSm %>%
  filter(bin <= 10000, n>20) %>%
  ggplot() +
    facet_grid(uvp_model~group) +
    geom_ribbon(aes(x=bin, ymin=q25, ymax=q75, fill=depth_bin), alpha=0.3) +
    geom_path(aes(x=bin, y=q50, colour=depth_bin), linewidth=0.5) +
    scale_x_log10(limits=c(0.1, 10000), sec.axis=sec_axis(trans=~2*(./pi*3/4)^(1/3) , name="Object ESD (mm)")) +
    scale_y_log10() +
    scale_colour_manual(values=blues) + scale_fill_manual(values=blues) +
    labs(x="Object volume (mm3)", y="Normalised volume (mm3/L/mm3)", colour="Depth layer", fill="Depth layer") +
    theme(panel.grid.minor=element_blank(), legend.position="inside", legend.position.inside=c(0.95, 0.5))

ggsave("plots/size_spectra.pdf", width=w2, height=w, unit="cm")


## Fig: Concentration map ----

# compute concentrations, biovolumes, etc. in consistent depth levels
props <- obj %>%
  filter(!group %in% c("bubble")) %>%
  mutate(group=ifelse(group %in% c("Not plankton"), "not_plankton", "plankton")) %>%
  properties_per_bin(vol, depth_breaks=c(0, 100, 500, 1000)) %>%
  left_join(select(smp, sample_id, lon, lat), by="sample_id")

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
(map_det + labs(size="Concentration\nof not plankton\n(mostly detritus) [#/L]")) + (map_plank + labs(size="Concentration\nof plankton [#/L]"))
# and save the result
ggsave("plots/map_concentrations.pdf", width=w2, height=w, unit="cm")


## Convert plots into png images for Google Docs ----

pdfs <- list.files("plots", pattern="pdf", full.names=TRUE)
walk(pdfs, function(f) { system(str_c("convert -density 300 ", f, " ", str_replace(f, "pdf", "png"))) } )
