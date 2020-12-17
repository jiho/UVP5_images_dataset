#
# Extract sampled volume to be able to compute concentrations
#
# (c) 2020 Jean-Olivier Irisson, GNU General Public License v3

source("0.setup.R")

# read selected samples
samples <- read_tsv("data/UVP5_samples_selected.tsv", col_types=cols()) %>%
  select(sampleid, psampleid)

volumes <- samples %>%
  left_join(tbl(db, "part_histopart_reduit") %>% select(psampleid, mid_depth_bin=depth, water_volume_imaged=watervolume), copy=TRUE) %>%
  select(-psampleid) %>%
  collect()

summary(volumes)

write_tsv(volumes, "data/UVP5_volumes.tsv.gz")
