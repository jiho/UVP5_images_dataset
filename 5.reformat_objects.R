#
# Reformat the total objects data
#
# (c) 2020 Jean-Olivier Irisson, GNU General Public License v3

source("0.setup.R")
library("arrow")
library("ecotaxar")


# read sample info
samples <- read_tsv("data/UVP5_samples_selected.tsv", col_types=cols())

# read object-level data from the files saved on disk
proj_files <- list.files(proj_dir, pattern="feather", full.names=TRUE)
o <- map_dfr(proj_files, read_feather)

nrow(o)
# 7,186,461 on 2020-12-16 00:16
# 7,820,053 on 2021-02-13 00:31 (added dubious objects and some additional profiles)
# 8,565,863 on 2023-12-29 22:47


## Restrict to relevant data -----

summary(o$imgrank)

o <- o %>%
  # remove columns that are only in a few projects
  select(-(rawvig:image_index)) %>%
  # remove imgrank, we only have 0
  select(-imgrank)

# keep only objects for which we have a corresponding water volume
# indeed, when we have no volume the data is useless
# missing volumes can be dues to:
# - changes (in e.g. firstimgok) between the times when particles were counted and when objects were extracted
# - different handling of particles and large objects (like it was the case at the beginning)
# - a volume that spiked and was therefore removed at a previous step
volume <- read_tsv("data/UVP5_volumes.tsv.gz", col_types=cols())
# shift depth by the depth offset
o <- o %>%
  left_join(select(samples, sampleid, depth_offset), by="sampleid") %>%
  mutate(depth=depth+depth_offset)
# match with water volume imaged by depth bin
o_wv <- o %>%
  mutate(mid_depth_bin=floor(depth/5)*5 + 2.5) %>%
  inner_join(volume)

nrow(o) - nrow(o_wv)
# removed 210,451 objects

# remove objects with tag == 2 which correspond to faulty lights and/or upward movement during the downcast
o_t <- filter(o_wv, tag != 2)

nrow(o_wv) - nrow(o_t)
# removed 0 additional objects
# TODO tag=2 does not seem set on the projects that need it

# remove objects corresponding to upward movement during the downcast
# (that were not removed by the tag=2 filter above)
o_up <- o_t %>%
  # extract the image number within the cast
  mutate(img_nb=str_replace(origid, "^.*_", "") %>% as.numeric()) %>%
  # for each cast
  group_by(sampleid) %>%
  # sort objects in increasing order
  # = in the order they were captured
  arrange(img_nb) %>%
  # keep only images that are at or below the maximum depth already experienced
  # = filter out the upward movements
  filter(depth >= cummax(depth)) %>%
  ungroup()

nrow(o_t) - nrow(o_up)
# removed 84,741 additional rows

# save the cleaned version
oc <- o_up


## Enrich data with extra fields ----

# add taxonomic names (unique at the level of the whole dataset)
taxo <- extract_taxo(dbt, oc$classif_id)
oc$taxon <- taxo_name(oc$classif_id, taxo, unique=TRUE)
oc$lineage <- lineage(oc$classif_id, taxo)

# identify users through names and emails
userids <- oc$classif_who_all %>% unlist() %>% unique() %>% as.integer()
users <- tbl(dbt, "users") %>%
  select(id, name, email) %>%
  filter(id %in% !!userids) %>%
  collect() %>%
  mutate(user=str_c(str_to_title(name), " <", email, ">"))
oc$annotators <- map(oc$classif_who_all, function(x) {users$user[match(x, users$id)]})

# compute sizes in human understandable units
oc <- left_join(oc, select(samples, sampleid, acq_pixel), by="sampleid") %>%
  mutate(
    area_mm2 = area * acq_pixel^2,
    esd_mm = 2 * sqrt(area_mm2 / pi),
    vol_mm3 = 4/3 * pi * (esd_mm^3),
    length_mm = major * acq_pixel
  )

# define UVP model
oc <- oc %>%
  mutate(
    uvp_model=case_when(
      pixel_size < 0.1 ~ "HD",
      pixel_size > 0.14 ~ "SD",
      TRUE ~ "ZD"
    )
  )

## Prepare taxonomic regrouping ----

# compute total per taxon

## Extract information to help re-sorting ----

filter(o, str_detect(lineage, "Dinophyceae")) %>% count(projid)
filter(o, taxon == "Harosa") %>% count(projid)
filter(o, taxon == "Rhizaria X") %>% count(projid)
filter(o, taxon == "rhizaria like") %>% count(projid)
filter(o, taxon == "solitaryblack-like") %>% count(projid)
filter(o, taxon == "Arthropoda") %>% count(projid)
filter(o, str_detect(taxon, "Tunicata")) %>% count(projid)
filter(o, str_detect(taxon, "Enteropneusta")) %>% count(projid)
filter(o, taxon == "darksphere") %>% count(projid)
filter(o, str_detect(taxon, "t0")) %>% count(taxon, projid) %>% filter(n>500)
count(oc, lineage, taxon) %>%
  write_tsv("data/UVP5_taxo.tsv", na="")


## Write data to disk ----

# remove irrelevant variables and reorder columns
select(oc,
       # identifiers
       projid, sampleid, objid, origid,
       # image
       file_name,
       # taxonomy
       taxon, lineage,
       # annotators
       annotators, last_annotator_id=classif_who, last_annotation_date=classif_when,
       # depth
       depth, mid_depth_bin,
       # measurements in human readable units
       contains("_mm"),
       # zooprocess descriptors
       area:skeleton_area,
  ) %>%
  # add save to disk for now
  write_feather(path=file.path(data_dir, "all.feather"))
