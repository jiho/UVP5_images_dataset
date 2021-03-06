#
# Read and reformat all intermediate files to make the final dataset
#
# (c) 2021 Jean-Olivier Irisson, GNU General Public License v3

# create an output folder for the final dataset
dir.create("data/final", showWarnings=FALSE)

## Sample level information ----

# read selected samples
samples <- read_tsv("data/UVP5_samples_selected.tsv", col_types=cols())
# reformat to write it to the final file
samples %>%
  select(
    sampleid, profile,
    lat, lon, datetime, pixel_size=acq_pixel
  ) %>%
  write_tsv(file="data/final/samples.tsv.gz")

# read water volume
volume <- read_tsv("data/UVP5_volumes.tsv.gz", col_types=cols())
volume %>%
  # reformat (or not!), to write it to the final file
  write_tsv(file="data/final/samples_volume.tsv.gz")
