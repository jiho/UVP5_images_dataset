#
# Extract sampled volume to be able to compute concentrations
#
# (c) 2020 Jean-Olivier Irisson, GNU General Public License v3

source("0.setup.R")
# remotes::install_github("jiho/castr")
library("castr")

# read selected samples
samples <- read_tsv("data/UVP5_samples_selected.tsv", col_types=cols()) %>%
  select(sampleid, psampleid)

volumes <- samples %>%
  left_join(tbl(dbp, "part_histopart_reduit") %>% select(psampleid, mid_depth_bin=depth, water_volume_imaged=watervolume), copy=TRUE) %>%
  select(-psampleid) %>%
  arrange(sampleid, mid_depth_bin) %>%
  collect()

summary(volumes)
# -> some very high volumes

# despike the volume imaged
# = detect large changes in water_volume_imaged and consider those to be either unrealistic or regions of over sampling => set them to NA
volumes <- volumes %>%
  group_by(sampleid) %>%
  mutate(water_volume_clean=despike(water_volume_imaged,
                                    # make window size a function of profile length
                                    k=as.integer(sqrt(n())),
                                    # run it just once
                                    n.max=2, mult=15)) %>%
  ungroup()

# inspect again
summary(volumes)
sum(is.na(volumes$water_volume_clean)) / nrow(volumes) * 100
# [1] 0.5036592
# = we removed less than 1% of the data bins


# look at random profiles
# filter(volumes, sampleid %in% sample(unique(volumes$sampleid), 30)) %>%
# look at profiles with still some spikes in the volume
filter(volumes, sampleid %in% unique(filter(volumes, water_volume_clean>400)$sampleid)) %>%
  ggplot() + facet_wrap(~sampleid, scales="free") +
  geom_path(aes(x=water_volume_imaged, y=-mid_depth_bin), colour="red") +
  geom_path(aes(x=water_volume_clean, y=-mid_depth_bin), colour="black") +
  geom_point(aes(x=water_volume_clean, y=-mid_depth_bin), colour="black", size=0.5) +
  coord_cartesian(xlim=c(0,1000))

# OK, the anomaly detector is quite good, let us drop those volumes
volumes <- volumes %>%
  drop_na(water_volume_clean) %>%
  select(-water_volume_clean)

write_tsv(volumes, "data/UVP5_volumes.tsv.gz")
