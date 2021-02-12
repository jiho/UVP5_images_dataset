#
# Fetch almost fully validated UVP5 casts and their metadata
#
# (c) 2020 Jean-Olivier Irisson, GNU General Public License v3

# NB: some bits are long, run as a job

source("0.setup.R")

## Compute candidate samples status ----

# read selected projects
# projects <- read_csv("https://docs.google.com/spreadsheets/d/1J3ldlk5tU2rgb7aav61FsNXOc8DbRPzankByfVr0Xj0/export?format=csv")
projects <- read_csv("data/UVP5_projects_selected.csv")
selected_projects <- projects %>% filter(use != "")

# get sorting status of corresponding samples
samples <- tbl(db, "part_samples") %>%
  # get ecotaxa sampleids from selected particle projects
  filter(pprojid %in% !!selected_projects$pprojid) %>%
  select(psampleid, sampleid, profile=profileid) %>%
  # get classification status of objects in those
  left_join(tbl(db, "objects") %>% select(sampleid, classif_qual), by="sampleid") %>%
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


## Compare with the original selection by Thelma ----

# read the samples used by Thelma
thelma_samples <- read_csv("data/list_profiles_ecopart-thelma.csv", col_types=cols()) %>%
  select(title=ecotaxa, ptitle=ecopart, profile=profiles) %>%
  separate_rows(profile, sep=",")

# get our current selection of samples
current_samples <- selected_samples_info %>%
  select(ends_with("id"), profile) %>%
  # add projects titles
  left_join(select(projects, pprojid, projid, ptitle, title), by=c("pprojid", "projid"))

# profiles in Thelma's selection but not in ours
missing_in_current <- filter(thelma_samples, ! profile %in% current_samples$profile)

missing_in_current %>%
  left_join(samples_validation_status) %>%
  select(title, sampleid, profile, percent_validated) %>%
  arrange(title, profile)
# -> four samples (an1304_l2_002, 4, 5, 6) are missing in EcoPart now...
#    uvp5_sn009_2015_p16n now has no images in it
#    c_msm22_087 has lots of dubious

# write those and finish sorting them
# missing_in_current %>% group_by(title) %>% summarise(samples=str_c(profile, collapse=",")) %>% write_tsv("to_finish_sorting.tsv")

# profiles in our selection but not in Thelma's
extra_in_current <- filter(current_samples, ! profile %in% thelma_samples$profile)

# check the project names to make sure we do not have extra projects
# NB: this has to be done by eye, since some of the project names have changed...
unique(extra_in_current$title) %>% sort()
unique(thelma_samples$title) %>% sort()
# -> nope, that seems OK

extra_in_current %>%
  left_join(samples_validation_status) %>%
  select(title, sampleid, profile, percent_validated) %>%
  arrange(title, profile)
# -> 79 extra profiles
#    some are probably due to the 99% instead of 100% validated criterion
#    others probably have been sorted since
