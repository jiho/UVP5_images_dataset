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

# TODO fix this in the data
library("lubridate")
samples$datetime[year(samples$datetime)==1987] <- samples$datetime[year(samples$datetime)==1987] + years(2016-1987)
filter(samples, projid==149)$datetime


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

source("data/final/compute_concentrations_biovolumes.R", chdir=TRUE)
