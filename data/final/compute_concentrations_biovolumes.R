#
# Compute concentration and biovolume from UVP data
# per taxonomic group and depth bin
#
# (c) 2024 Jean-Olivier Irisson, GNU General Public License v3

library("dplyr")
library("readr")
library("stringr")

# read object identifications
# NB: keep only the columns of interest
obj <- read_tsv("objects.tsv.gz", col_select=c("sample_id", "depth", "group", "vol_mm3"), show_col_types=FALSE)

# read sampled water volume
vol <- read_tsv("samples_volume.tsv.gz", show_col_types=FALSE)

# define broad depth bins (as in EcoPart)
ecopart_bins <- c(0, 25, 50, 75, 100, 125, 150, 200, 250, 300, 350, 400, 450, 500, 600, 700, 800, 900, 1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750, 3000, 3250, 3500, 3750, 4000, 4250, 4500, 4750, 5000, 5250, 5500, 5750, 6000, 7000, 8000, 9000, 10000, 11000, 12000, 13000, 14000, 15000, 20000, 50000)

# compute sampled volume per broad depth bin
vol_per_bin <- vol %>%
  # cut into EcoPart depth bins
  mutate(ecopart_depth_bin=cut(mid_depth_bin, breaks=ecopart_bins, include.lowest=TRUE, dig.lab=5)) %>%
  # compute total volume imaged in this new bin
  group_by(sample_id, ecopart_depth_bin) %>%
  summarise(water_volume_imaged=sum(water_volume_imaged), max_depth=max(mid_depth_bin)+2.5, .groups="drop") %>%
  # remove incompletely sampled bins
  mutate(complete=str_detect(as.character(ecopart_depth_bin), str_c(max_depth, "]"))) %>%
  filter(complete) %>%
  select(-max_depth, -complete)

# compute concentration and biovolume per depth bin
res <- obj %>%
  # cut into the same depth bins as above
  mutate(ecopart_depth_bin=cut(depth, breaks=ecopart_bins, include.lowest=TRUE, dig.lab=5)) %>%
  # force the taxonomic group to be a factor
  # (this ensures that every group is considered at every depth bin of every profile)
  mutate(group=factor(group)) %>%
  # for each depth bin of each profile, for each group (including those absent)
  group_by(sample_id, ecopart_depth_bin, group, .drop=FALSE) %>%
  # compute abundance and total volume
  summarise(n=n(), vol=sum(vol_mm3), .groups="drop") %>%
  # add water volume sampled
  inner_join(vol_per_bin) %>% # NB: using inner_join reduces to completely sampled bins
  # and compute concentratins and biovolumes
  mutate(
    concentration=n/water_volume_imaged,
    biovolume=vol/water_volume_imaged,
  ) %>%
  # re-order, subset and rename columns
  select(sample_id, depth_range=ecopart_depth_bin, group, concentration, biovolume)

# write the final data set to disk
write_tsv(res, file="concentrations_biovolumes.tsv.gz")
