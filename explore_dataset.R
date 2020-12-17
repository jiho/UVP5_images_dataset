#
# Explore data set properties and extract nice figures
#
# (c) 2020 Jean-Olivier Irisson, GNU General Public License v3

library("tidyverse")
library("chroma")
dir.create("figures", showWarnings=FALSE)

s <- read_tsv("dataset/samples.tsv.gz")
coast <- read_csv("data/gshhg_world_c.csv.gz")

nrow(s)

ggplot() + coord_quickmap() + scale_xy_map() +
  geom_polygon(aes(lon, lat), data=coast, fill="grey60") +
  geom_point(aes(lon, lat), data=s, size=0.1)
ggsave("figures/map.pdf", width=6, heigh=3.5)
