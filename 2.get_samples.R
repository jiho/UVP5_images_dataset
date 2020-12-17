#
# Fetch almost fully validated UVP5 casts and their metadata
#
# (c) 2020 Jean-Olivier Irisson, GNU General Public License v3

# NB: some bits are long, run as a job

source("0.setup.R")

## Compute candidate samples status ----

# read selected projects
selected_projects <- read_csv("https://docs.google.com/spreadsheets/d/1J3ldlk5tU2rgb7aav61FsNXOc8DbRPzankByfVr0Xj0/export?format=csv") %>%
  filter(use != "")
# TODO replace by a static .tsv export when this is stable

# get sorting status of corresponding samples
samples <- tbl(db, "part_samples") %>%
  # get ecotaxa sampleids from selected particle projects
  filter(pprojid %in% !!selected_projects$pprojid) %>%
  select(psampleid, sampleid, profile=profileid) %>%
  # get classification status of objects in those
  left_join(tbl(db, "obj_head") %>% select(sampleid, classif_qual), by="sampleid") %>%
  count(profile, sampleid, classif_qual) %>%
  ungroup() %>% collect()
# NB: this takes ~5 min

# compute sorting percentage for those
samples_validation_status <- samples %>%
  # compute sorting percentage
  pivot_wider(names_from=classif_qual, values_from=n, values_fill=0) %>%
  rename(N=`NA`) %>%
  mutate(
    total=V+D+P+N,
    percent_validated=V/total*100
  )


## Extract a selection of samples ----

# keep only samples almost fully sorted
selected_samples <- samples_validation_status %>%
  filter(percent_validated > 99)

# extract sample level information (from EcoPart because this is the most reliable source)
selected_samples_info <- tbl(db, "part_samples") %>%
  filter(sampleid %in% !!selected_samples$sampleid) %>%
  select(
    # identifiers
    pprojid, psampleid, sampleid, profile=profileid,
    # location
    lat=latitude, lon=longitude, datetime=sampledate,
    # relevant acquisition metadata
    acq_aa, acq_exp, acq_depthoffset, acq_pixel,
    # extra
    comment
  ) %>%
  # compute depth offset from project level default and sample level one
  left_join(tbl(db, "part_projects") %>% select(pprojid, default_depthoffset)) %>%
  mutate(depth_offset=ifelse(is.na(acq_depthoffset), default_depthoffset, acq_depthoffset)) %>%
  select(-acq_depthoffset, -default_depthoffset) %>%
  collect() %>%
  # add EcoTaxa project id
  left_join(select(selected_projects, pprojid, projid)) %>%
  relocate(projid, .before="sampleid")

# write it to a file
write_tsv(selected_samples_info, "data/UVP5_samples_selected.tsv")
