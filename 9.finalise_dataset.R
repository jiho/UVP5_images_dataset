#
# Read and reformat all intermediate files to make the final dataset
#
# (c) 2021 Jean-Olivier Irisson, GNU General Public License v3

source("0.setup.R")
library("arrow")
library("data.table") # use fwrite for writing files because it is much faster

# create an output folder for the final dataset
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
samples %>%
  left_join(select(projects, pprojid, ptitle)) %>%
  select(
    project=ptitle, sample_name=profile_name, sample_id=sampleid,
    lat, lon, datetime, pixel_size=acq_pixel
  ) %>%
  # define UVP model
  mutate(
    uvp_model=case_when(
      pixel_size < 0.1 ~ "HD",
      pixel_size > 0.14 ~ "SD",
      TRUE ~ "ZD"
    )
  ) %>%
  fwrite(file="data/final/samples.tsv.gz", sep="\t", na="NA")

## Volume table ----

# read water volume
volume <- read_tsv("data/UVP5_volumes.tsv.gz", col_types=cols())
volume %>%
  rename(sample_id=sampleid) %>%
  fwrite(file="data/final/samples_volume.tsv.gz", sep="\t", na="NA")


## Object table -----

o <- read_feather(file.path(data_dir, "all.feather"))
projects <- read_tsv("data/UVP5_projects_selected.tsv", col_types=cols())

o_s <- o %>%
  # add "anonymized" user names
  mutate(classification_author=paste0("user_", last_annotator_id)) %>%
  # NB: user paste0 and not str_c because there are NA users
  # select and reorganise columns
  select(
    # link with profile
    sample_id=sampleid,
    depth, mid_depth_bin,
    # identifiers
    object_name=objname, object_id=objid,
    # taxo
    taxon, lineage, group, group_lineage,
    classification_author, classification_date=last_annotation_date,
    # features
    contains("_mm"),
    area:skeleton_area
  )

fwrite(o_s, file="data/final/objects.tsv.gz", sep="\t", na="NA")


## Concentrations table ----

# check data quality
sum(is.na(o_s$vol_mm3))

all_groups <- unique(o_s$group) %>% sort()
all <- crossing(select(volume, profile_id, mid_depth_bin), all_groups)
nrow(volume) * 32
# tooo many!

# TODO reduce to larger depth bins, like on EcoPart

per_group <- o_s %>%
  # restrict to the variables of interest
  select(profile_id, mid_depth_bin, group, vol_mm3) %>%
  # force the taxonomic group to be a factor,
  # to ensure that every group is considered at every depth bin of every profile
  mutate(group=factor(group)) %>%
  # for each depth bin of each profile,
  group_by(profile_id, mid_depth_bin, group, .drop=FALSE) %>%
  summarise(n=n(), vol=sum(vol_mm3), .groups="drop") %>%
  left_join(volume) %>%
  mutate(
    concentration=n/water_volume_imaged,
    biovolume=vol/water_volume_imaged,
  ) %>%
  select(profile_id, depth=mid_depth_bin, group, concentration, biovolume)


tibble(a=c(1,2,1), b=c(1,1,2), c=factor(c(1,2,2))) %>%
  group_by(a, b, c, .drop=FALSE) %>%
  summarise(n=n(), .groups="drop")


# TODO isolate the code in a separate function, that is in the dataset
