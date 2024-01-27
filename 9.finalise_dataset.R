#
# Read and reformat all intermediate files to make the final dataset
#
# (c) 2021 Jean-Olivier Irisson, GNU General Public License v3

source("0.setup.R")
library("arrow")
library("data.table") # use fwrite for writing files because it is much faster

# create an output folder for the final data set (if it does not exist already)
dir.create("data/final", showWarnings=FALSE)


## Sample table ----

# read selected samples
samples <- read_tsv("data/UVP5_samples_selected.tsv", col_types=cols())
projects <- read_tsv("data/UVP5_projects_selected.tsv", col_types=cols())

# reformat to write it to the final file
smp <- samples %>%
  left_join(select(projects, pprojid, ptitle)) %>%
  select(
    sample_id=sampleid, sample_name=profile_name, project=ptitle,
    lat, lon, datetime, pixel_size=acq_pixel
  ) %>%
  # define UVP model
  mutate(
    uvp_model=case_when(
      pixel_size < 0.1 ~ "HD",
      pixel_size > 0.14 ~ "SD",
      TRUE ~ "ZD"
    )
  )
fwrite(smp, file="data/final/samples.tsv.gz", sep="\t", na="NA")

## Volume table ----

# read water volume
volume <- read_tsv("data/UVP5_volumes.tsv.gz", col_types=cols())
vol <- volume %>%
  rename(sample_id=sampleid)
fwrite(vol, file="data/final/samples_volume.tsv.gz", sep="\t", na="NA")


## Object table -----

o <- read_feather(file.path(data_dir, "all.feather"))
projects <- read_tsv("data/UVP5_projects_selected.tsv", col_types=cols())

obj <- o %>%
  # add "anonymized" user names
  mutate(classif_author=paste0("user_", last_annotator_id)) %>%
  # NB: user paste0 and not str_c because there are NA users
  # select and reorganise columns
  select(
    # identifiers
    object_id=objid, object_name=objname,
    # link with profile
    sample_id=sampleid,
    depth, mid_depth_bin,
    # taxo
    taxon, lineage,
    classif_author, classif_datetime=last_annotation_date,
    group, group_lineage,
    # features
    contains("_mm"),
    area:skeleton_area
  )

fwrite(obj, file="data/final/objects.tsv.gz", sep="\t", na="NA")


## Concentrations/biovolume table ----

# check data quality
sum(is.na(obj$vol_mm3))
sum(is.na(vol$water_volume_imaged))
# -> OK

# get the function to compute the concentrations/biovolumes
source("data/final/compute_concentrations_biovolumes.R")

# define broad depth bins (as in EcoPart)
ecopart_bins <- c(0, 25, 50, 75, 100, 125, 150, 200, 250, 300, 350, 400, 450, 500, 600, 700, 800, 900, 1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750, 3000, 3250, 3500, 3750, 4000, 4250, 4500, 4750, 5000, 5250, 5500, 5750, 6000, 7000, 8000, 9000, 10000, 11000, 12000, 13000, 14000, 15000, 20000)

res <- properties_per_bin(obj, vol, depth_breaks=ecopart_bins)

# write the final data set to disk
write_tsv(res, file="data/final/concentrations_biovolumes.tsv.gz")

