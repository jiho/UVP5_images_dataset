#
# Read and reformat all intermediate files to make the final dataset
#
# (c) 2021 Jean-Olivier Irisson, GNU General Public License v3

library("data.table") # use fwrite for writing files because it is much faster
# create an output folder for the final dataset
dir.create("data/final", showWarnings=FALSE)

## Sample table ----

# read selected samples
samples <- read_tsv("data/UVP5_samples_selected.tsv", col_types=cols())
projects <- read_tsv("data/UVP5_projects_selected.tsv", col_types=cols())
# reformat to write it to the final file
samples %>%
  left_join(select(projects, pprojid, ptitle)) %>%
  select(
    project=ptitle, profile, profile_id=sampleid,
    lat, lon, datetime, pixel_size=acq_pixel
  ) %>%
  mutate(
    uvp_model=case_when(
      pixel_size < 0.1 ~ "HD",
      pixel_size > 0.14 ~ "SD",
      TRUE ~ "ZD"
    )
  ) %>%
  fwrite(file="data/final/samples.tsv.gz", sep="\t", na="NA")

# read water volume
volume <- read_tsv("data/UVP5_volumes.tsv.gz", col_types=cols())
volume %>%
  # reformat (or not!), to write it to the final file
  write_tsv(file="data/final/samples_volume.tsv.gz")
