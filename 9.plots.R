#
# Read the final dataset and make some plots
#
# (c) 2021 Jean-Olivier Irisson, GNU General Public License v3

library("tidyverse")

## Read data ----

# coastline
coast <- read_csv("data/gshhg_world_l.csv.gz", col_types=cols())

# sample level
smps <- read_tsv("data/final/samples.tsv.gz")
vols <- read_tsv("data/final/samples_volume.tsv.gz")

#objects level
objs_full <- read_tsv("data/final/objects.tsv.gz")
objs <- objs_full[, c('profile_id', 'object_id', 'depth', 'group', 'group_lineage')]

## Table of images of consistent categories ----
# morphological PCA per category and pick something in the middle


## Map ----

(p_map <- ggplot(mapping=aes(lon, lat)) +
  # coastline
  # NB: coord_map does weird things to the polar polygons...
  coord_quickmap() +
  geom_polygon(data=coast, fill="grey99") +
  # profiles
  geom_point(aes(shape=uvp_model), data=smps, size=1, alpha=0.5) +
  scale_shape_manual("", values=c(4,1,17)) +
  # nice breaks and labels
  scale_x_continuous(
    minor_breaks=NULL, expand=c(0,0),
    labels=function(x) {paste0(abs(x), "°", c("W", "", "E")[sign(x) + 2])}
  ) +
  scale_y_continuous(
    breaks=c(-60, -30, 0, 30, 60), minor_breaks=NULL, expand=c(0,0),
    labels=function(x) {paste0(abs(x), "°", c("S", "", "N")[sign(x) + 2])}
  ) +
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

## Temporal distribution ----

# time series of number of profiles per month

# library("lubridate")
#
# # compute a regular sequence of dates
# smps$date <- as.Date(smps$datetime)
# start <- min(smps$date)
# end   <- max(smps$date) + months(1)
# dates <- seq(start, end, by="1 month")
# day(dates) <- 15
#
# # compute the number of profiles per month
# # and add zeroes
# day(smps$date) <- 15
# ts <- left_join(
#     tibble(date=dates),
#     count(smps, date)
#   ) %>%
#   replace_na(list(n=0))
#
# # plot
# ts %>%
#   ggplot() +
#   geom_line(aes(x=date, y=n))

# simple time line
# with some latitude resolution
(p_ts <- ggplot(smps) +
  geom_point(aes(x=datetime, y=cut(lat, c(-90,-30,30,90))),
             shape="|", alpha=0.1, size=4)+
  labs(x="Date of profile", y="Latitude"))


## Depth distribution ----

# compute maximum depth per profile and add it to the samples table
smps <- left_join(
  smps,
  vols %>%
    group_by(profile_id) %>%
    summarise(max_depth=max(mid_depth_bin)) %>%
    ungroup()

)

# sqrt transform for negative values
# (copied and modified from sqrt_trans in scales/R/trans-numeric.r)
neg_sqrt_trans <- function() {
  scales::trans_new("sqrt",
                    function(x) {-sqrt(-x)},
                    function(x) -(x^2),
                    domain = c(0, Inf))
}

(p_depth <- ggplot(smps) +
  geom_histogram(aes(y=-max_depth), binwidth=1) +
  scale_y_continuous(
    # nice breaks
    trans="neg_sqrt",
    breaks=-c(1, 500, 1000, 2000, 4000, 6000),
    # put the legend on the right for better composition
    position="right"
  ) +
  labs(x="Nb of profiles", y="Maximum depth [m]"))


## Combine the three plots ----

library("patchwork")

# layout the plot
layout <- "
AAAAB
AAAAB
AAAAB
CCCCB
"
p_map + p_depth + p_ts +plot_layout(design=layout)

# and save
ggsave(file="data/final/plot_samples.pdf", width=19*1.5, height=9*1.5, unit="cm")

# God do I love patchwork!














## Histogram of the size of the ~five most abundant taxa + detritus separating 0:200m and 200m:bottom ----
# should use bin=[0, 25, 50, 100, 150, 200, 250, 300, 400, 500, 600, 800, 1000, 2000, 3000, 5000]

# get 0-200m data #layer1
p_totals_by_groups_l1 <-objs %>%
  filter(depth<=200) %>%
  filter(startsWith(group_lineage, 'living')) %>%
  count(group) %>%
  arrange(desc(n)) %>%
  head(15)%>%
  ggplot(aes(y=reorder(group, n), x=n))+
  geom_bar(stat="identity")+
  ggtitle("Fifteen most abundant taxa : 0m to -200m")+
  xlab("Nb of objects")+
  ylab("Taxa")#+
  #scale_x_log10()
p_totals_by_groups_l1

totals_by_groups_l1 <- objs %>%
  filter(depth<=200) %>%
  filter(startsWith(group_lineage, 'living') | startsWith(group, 'detritus')) %>%
  count(group) %>%
  arrange(desc(n)) %>%
  head(6)

p_obj_size_l1 <- objs %>%
  filter(depth<=200) %>%
  filter(grepl(paste(totals_by_groups_l1$group, collapse="|"), group))%>%
  ggplot(aes(y=-depth, fill=group)) +
  geom_histogram(binwidth = 1 , alpha=1) +
  ggtitle("Size histogram of the five most abundant taxa and detritus : 0m to -200m")+
  xlab("Nb of objects")+
  ylab("Depth [m]")+
  scale_fill_manual(values=c("#CF90DE", "#FFF393", "#6058DB", "#CBDEAF", "#8EC2DE", '#FD8A6A')) +
  scale_y_continuous(breaks=c(0, -25, -50, -100, -150, -200))+
  facet_wrap(~factor(group, levels=c(totals_by_groups_l1$group)), scale="free_x")
p_obj_size_l1


# get 200-bottom data #layer2
p_totals_by_groups_l2 <-objs %>%
  filter(depth>200) %>%
  filter(startsWith(group_lineage, 'living')) %>%
  count(group) %>%
  arrange(desc(n)) %>%
  head(15)%>%
  ggplot(aes(y=reorder(group, n), x=n))+
  geom_bar(stat="identity")+
  ggtitle("Fifteen most abundant taxa : -200m to Bottom")+
  xlab("Nb of objects")+
  ylab("Taxa")
p_totals_by_groups_l2

totals_by_groups_l2 <- objs %>%
  filter(depth>200) %>%
  filter(startsWith(group_lineage, 'living') | startsWith(group, 'detritus')) %>%
  count(group) %>%
  arrange(desc(n)) %>%
  head(6)

p_obj_size_l2 <- objs %>%
  filter(depth>200) %>%
  filter(grepl(paste(totals_by_groups_l2$group, collapse="|"), group))%>%
  ggplot(aes(y=-depth, fill=group)) +
  geom_histogram(binwidth = 1, alpha=1) +
  ggtitle("Size histogram of the five most abundant taxa and detritus : -200m to Bottom")+
  xlab("Nb of objects")+
  ylab("Depth [m]")+
  scale_fill_manual(values=c("#CF90DE", "#FFF393", "#6058DB", "#CBDEAF", "#8EC2DE", '#FD8A6A')) +
  scale_y_continuous(
    # nice breaks
    trans="neg_sqrt",
    breaks=c(-200,-250, -300, -400, -500, -600, -800, -1000, -2000, -3000, -5000)
  )+
  facet_wrap(~factor(group, levels=c(totals_by_groups_l2$group)), scale="free_x")
p_obj_size_l2


## Total concentration per taxon for the consistent categories ----
# ordered by concentration, over the whole dataset
# Barplot with total concentration per taxon for the consistent categories
tot_vol_m3=sum(vols$water_volume_imaged)/1000# From L to m3
p_concentration_by_taxon<-objs %>%
  filter(startsWith(group_lineage, 'living')) %>%
  count(group)%>%#go from nb indiv to concentration by dividing with tot_vol_m3
  mutate(n=n/tot_vol_m3) %>%
  arrange(desc(n)) %>%
  head(15)%>%
  ggplot(aes(y=reorder(group, n), x=n))+
  geom_bar(stat="identity")+
  ggtitle("Total concentration per taxon for the consistent categories, \nordered by concentration, over the whole dataset.")+
  xlab("Concentration (ind/m3)")+
  ylab("Taxa")
p_concentration_by_taxon

# over some depth bins
# layout the plot
layout_other <- "
A#B#
A#B#
CCDD
CCDD
CCDD
E###
E###
"
p_totals_by_groups_l1 + p_totals_by_groups_l2 + p_obj_size_l1 + p_obj_size_l2 + p_concentration_by_taxon +plot_layout(design=layout_other)

# and save
ggsave(file="data/final/plot_other.pdf", width=30*1.5, height=30*1.5, unit="cm")

## Size spectra ----
# for the most abundant taxa




