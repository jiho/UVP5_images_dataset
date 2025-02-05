#
# Fetch (almost) fully validated UVP5 samples and their metadata
#
# (c) 2020 Jean-Olivier Irisson, GNU General Public License v3

# NB: some bits are long, run as a job in RStudio

source("0.setup.R")

## Compute candidate samples status ----

# read selected projects, from the previous step
# projects <- read_csv("https://docs.google.com/spreadsheets/d/1CrR-5PdhQ09JSTU482HOkTjtCvXRmpngYDuYSwaGQAo/export?format=csv", col_types=cols())
# NB: read from the tsv now that the selection is stabilised
#     those correspond roughly to the data in Drago et al + Panaiotis et al
projects <- read_tsv("data/UVP5_projects_selected.tsv", show_col_types=FALSE)

selected_projects <- projects %>% filter(use != "")

# get all samples from those projects
samples <- tbl(dbp, "part_samples") %>%
  # get ecotaxa sampleids from selected particle projects
  filter(pprojid %in% local(selected_projects$pprojid)) %>%
  select(psampleid, sampleid, profile_name=profileid) %>%
  collect()

# check unicity
sum(duplicated(samples$sampleid))
(dup_samples <- samples$sampleid[duplicated(samples$sampleid)])
# the "duplicated" ones are NA = EcoPart samples not associated with EcoTaxa samples => remove them
samples <- drop_na(samples, sampleid)

# get sorting stats of those samples
samples_classif <- tbl(dbt, "objects") %>%
  # limit to samples of potential interest
  filter(sampleid %in% local(samples$sampleid)) %>%
  # get classification status of all objects in those
  count(sampleid, classif_qual) %>%
  ungroup() %>% collect()

# compute sorting percentage for those
samples_classif_stats <- samples_classif %>%
  # compute sorting percentage
  pivot_wider(names_from=classif_qual, values_from=n, values_fill=0) %>%
  rename(N=`NA`) %>%
  mutate(
    total=V+D+P+N,
    percent_validated=(V+D)/total*100
    # NB: we consider the dubious too, since several objects were reassessed as dubious in Tara samples since the initial data set extraction
  ) %>%
  # add the psampleid and profile_name
  left_join(samples, by="sampleid")

# check for unicity
sum(duplicated(samples$sampleid))
sum(duplicated(samples$psampleid))
# -> OK

## Extract a selection of samples ----

# keep only samples almost fully sorted
selected_samples <- samples_classif_stats %>%
  filter(percent_validated > 99)
nrow(selected_samples)
# 2025-02-05 3185

# extract sample level information (from EcoPart because this is the most reliable source)
selected_samples_info <- tbl(dbp, "part_samples") %>%
  filter(psampleid %in% local(selected_samples$psampleid)) %>%
  select(
    # identifiers
    pprojid, psampleid, sampleid, profile_name=profileid,
    # location
    lat=latitude, lon=longitude, datetime=sampledate,
    # relevant acquisition metadata
    acq_aa, acq_exp, acq_depthoffset, acq_pixel,
    # extra
    comment
  ) %>%
  # compute depth offset from project level default and sample level one
  left_join(tbl(dbp, "part_projects") %>% select(pprojid, default_depthoffset), by="pprojid") %>%
  mutate(depth_offset=ifelse(is.na(acq_depthoffset), default_depthoffset, acq_depthoffset)) %>%
  select(-acq_depthoffset, -default_depthoffset) %>%
  collect() %>%
  # add EcoTaxa project id
  left_join(select(selected_projects, pprojid, projid), by="pprojid") %>%
  relocate(projid, .before="sampleid") %>%
  arrange(sampleid)

# check for duplicates
sum(duplicated(selected_samples_info$sampleid))


## Compare with the original selection by Thelma and Laetitia ----

# read the samples used by Thelma and Laetitia
thelma_samples <- read_csv("data/list_profiles_ecopart-thelma.csv", show_col_types=FALSE) %>%
  select(title=ecotaxa, ptitle=ecopart, profile_name=profiles) %>%
  separate_rows(profile_name, sep=",")

laeti_samples <- read_csv("data/list_profiles-laetitia.csv", show_col_types=FALSE) %>%
  rename(ptitle=pproject, profile_name=profile)

# get our current selection of samples
current_samples <- selected_samples_info %>%
  select(ends_with("id"), profile_name) %>%
  # add projects titles
  left_join(select(projects, pprojid, projid, ptitle, title), by=c("pprojid", "projid"))

# profiles in Thelma's selection but not in ours
missing_wr_thelma <- filter(thelma_samples, ! profile_name %in% current_samples$profile_name) %>%
  left_join(samples_classif_stats, by="profile_name") %>%
  select(title, sampleid, profile_name, percent_validated) %>%
  arrange(title, profile_name)
# print(missing_wr_thelma, n=200)
# -> four samples (an1304_l2_002, 4, 5, 6) are missing in EcoPart now...
#    5 moose samples seem to have changed (low validated percentage), for no known reason
#    uvp5_sn009_2015_p16n now has no images in it

# do the same for Laetitia's selection
missing_wr_laeti <- filter(laeti_samples, ! profile_name %in% current_samples$profile_name) %>%
  left_join(samples_classif_stats, by=c("psampleid", "profile_name", "sampleid")) %>%
  select(ptitle, sampleid, profile_name, percent_validated) %>%
  arrange(ptitle, profile_name)
# print(missing_wr_laeti, n=200)
# -> same as above: missing from an1304

# profiles in our selection but not in Thelma's
extra_in_current <- filter(current_samples, ! profile_name %in% c(thelma_samples$profile_name, laeti_samples$profile_name))
nrow(extra_in_current)

# check the project names to make sure we do not have extra projects
# NB: this has to be done partly by eye, since some of the project names have changed...
#     we do not have titles for Laetitia's samples
setdiff(unique(extra_in_current$title),unique(thelma_samples$title)) %>% sort()
unique(thelma_samples$title) %>% sort()
# -> mostly adding GEOMAR cruises

# show the profiles
extra_in_current <- extra_in_current %>%
  left_join(samples_classif_stats, by=c("psampleid", "profile_name", "sampleid")) %>%
  select(title, sampleid, profile_name, percent_validated) %>%
  arrange(title, profile_name)
# print(extra_in_current, n=500)
# -> 111 extra profiles
#    new profiles in AN Arctique possibly balancing the "lost" ones
#    most are probably due to the 99% instead of 100% validated criterion

# remove the extra ones, to publish ~ exactly what Laeti and Thelma worked with
# but keep the AN Arctique ones since thos probably correspond to the missing ones above
to_remove <- filter(extra_in_current, !str_detect(title, "AN Arctique"))
selected_samples <- filter(selected_samples, ! sampleid %in% to_remove$sampleid)
nrow(selected_samples)

# write the final selection to a file
write_tsv(selected_samples_info, "data/UVP5_samples_selected.tsv")
