#
# Read and reformat all intermediate files to make the final dataset
#
# (c) 2021 Jean-Olivier Irisson, GNU General Public License v3

source("0.setup.R")
library("feather")
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
    project=ptitle, profile_name, profile_id=sampleid,
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
# TODO go back to sample or call this file differently

## Volume table ----

# read water volume
volume <- read_tsv("data/UVP5_volumes.tsv.gz", col_types=cols())
volume %>%
  rename(profile_id=sampleid) %>%
  fwrite(file="data/final/samples_volume.tsv.gz", sep="\t", na="NA")


## Object table -----

o <- read_feather(file.path(data_dir, "all.feather"))
projects <- read_tsv("data/UVP5_projects_selected.tsv", col_types=cols())

# evaluate contribution of annotators
# TODO use the new extraction of contributors including history
head(o)
contrib <- count(o, projid, annotator) %>%
  mutate(annotator=str_replace_na(annotator, "N/A")) %>%
  filter(n>100) %>%
  left_join(distinct(projects, projid, data_owner, title)) %>%
  arrange(data_owner, title, desc(n)) %>%
  select(data_owner, title, projid, annotator, n) %>%
  write_tsv(file="sorting_contributions.tsv")

contrib_summary <- contrib %>%
  group_by(data_owner, title, projid) %>%
  summarise(contrib=str_c(annotator, format(n, trim=TRUE, big.mark=","), sep=": ", collapse="\n  ")) %>%
  group_by(data_owner) %>%
  summarise(list=str_c(title, ":\n  ", contrib, collapse="\n\n")) %>%
  summarise(s=str_c(data_owner, list, sep=":\n",collapse="\n\n--\n\n"))
cat(contrib_summary$s, file="sorting_contributions_summary.txt")

# anonymise annotators
people <- distinct(o, annotator) %>%
  arrange(annotator) %>%
  mutate(classification_author=str_c("user_", 1:n()))

o_s <- o %>%
  # add anonymous user name
  left_join(people) %>%
  # select and reorganise columns
  select(
    # link with profile
    profile_id=sampleid,
    depth, mid_depth_bin,
    # identifier
    object=origid, object_id=objid,
    # taxo
    taxon, lineage, group, group_lineage,
    classification_author, classification_date=annotation_date,
    # features
    contains("_mm"),
    area:skeleton_area
  )

# remove zero variance variables
vars <- sapply(select(o_s, area:skeleton_area), var, na.rm=T)
vars_no_change <- names(vars)[vars < 10^-9]
o_s <- select(o_s, -all_of(vars_no_change))

# remove variables with too many NAs
nb_NAs <- sapply(o_s, function(x) {sum(is.na(x))}) %>% sort()
o_s <- select(o_s, -perimareaexc, -feretareaexc, -cdexc)
# NB: this probably comes from the definition of area_exc in the UVP as the "surface of holes", not the surface excluding holes; see https://sites.google.com/view/piqv/zooprocess

fwrite(o_s, file="data/final/objects.tsv.gz", sep="\t", na="NA")


## Concentrations table ----
# isolate the code in a separate function, that is in the dataset
