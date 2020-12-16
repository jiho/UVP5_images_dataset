#
# Fetch fully validated UVP5 casts and their metadata
#
# (c) 2020 Jean-Olivier Irisson, GNU General Public License v3

# NB: some bits are long, run as a job

source("0.setup.R")

# read selected projects
selected_projects <- read_csv("https://docs.google.com/spreadsheets/d/1J3ldlk5tU2rgb7aav61FsNXOc8DbRPzankByfVr0Xj0/export?format=csv") %>%
  filter(use != "")
# TODO replace by a static .tsv export when this is stable

# get sorting status of corresponding samples
sample_classif <- tbl(db, "part_samples") %>%
  # get ecotaxa sampleids from selected particle projects
  filter(pprojid %in% !!selected_projects$pprojid) %>%
  select(psampleid, sampleid) %>%
  # get classification status of objects in those
  left_join(tbl(db, "obj_head") %>% select(sampleid, classif_qual)) %>%
  count(sampleid, classif_qual) %>%
  collect()
# NB: this takes ~5 min

# select almost fully sorted samples
selected_samples <- sample_classif %>%
  # compute sorting percentage
  pivot_wider(names_from=classif_qual, values_from=n, values_fill=0) %>%
  mutate(
    total=V+D+P+`NA`,
    percent_validated=V/total*100
  ) %>%
  # keep only samples sorted at more than 98%
  filter(percent_validated > 98)

# extract sample level information (from EcoPart because this is the most reliable source)
samples <- tbl(db, "part_samples") %>%
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

nrow(samples)
# 3368 on 2020-12-15 18:37

# write it to a file
write_tsv(samples, "data/UVP5_selected_samples.tsv")
