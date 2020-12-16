#
# Reformat the total objects data
#
# (c) 2020 Jean-Olivier Irisson, GNU General Public License v3

source("0.setup.R")
library("feather")
library("data.tree")

# read sample info
samples <- read_tsv("data/UVP5_samples.tsv", col_types=cols())

# read object-level data from the files saved on disk
proj_files <- list.files(proj_dir, pattern="feather", full.names=TRUE)
o <- map_dfr(proj_files, read_feather)

nrow(o)
# 7,186,461 on 2020-12-16 00:16

# add taxonomic names (unique at the level of the whole dataset)
taxo <- extract_taxo(db, o$classif_id)
o$taxon <- taxo_name(o$classif_id, taxo, unique=TRUE)
o$lineage <- lineage(o$classif_id, taxo)

# add user names
userids <- unique(o$classif_who) %>% as.integer()
users <- tbl(db, "users") %>%
  select(id, email) %>%
  filter(id %in% !!userids) %>%
  collect()
o$annotator <- users$email[match(o$classif_who, users$id)]

# shift depth by the depth offset
o <- o %>%
  left_join(select(samples, sampleid, depth_offset), by="sampleid") %>%
  mutate(depth=depth+depth_offset)

# compute sizes in human understandable units
o <- left_join(o, select(samples, sampleid, acq_pixel), by="sampleid") %>%
  mutate(
    area_mm2 = area * acq_pixel^2,
    esd_mm = 2 * sqrt(area_mm2 / pi),
    vol_mm3 = 4/3 * pi * (esd_mm^3),
    length_mm = major * acq_pixel
  )

# keep only objects for which we have a corresponding water volume
volume <- read_tsv("data/UVP5_volumes.tsv.gz", col_types=cols())
o <- o %>%
  mutate(mid_depth_bin=floor(depth/5)*5 + 2.5) %>%
  inner_join(volume)

# compute total per taxon
tc <- count(o, taxon, lineage) %>%
  # convert it into a tree
  rename(pathString=lineage) %>%
  arrange(pathString) %>%
  as.Node()
tcd <- ToDataFrameTree(tc, "taxon", "n") %>% mutate(group="")
# write the tree as a table
write_tsv(tcd, "data/UVP5_taxo.tsv", na="")

# remove irrelevant variables and reorder columns
select(o,
    # identifiers
    projid, sampleid, objid, origid, file_name,
    # taxonomy
    taxon, lineage, annotator, annotation_date=classif_when,
    # depth
    depth, mid_depth_bin,
    # measurements
    contains("_mm"),
    # zooprocess descriptors
    area:skeleton_area,
  ) %>%
  # add save to disk for now
  write_feather(path=file.path(data_dir, "all.feather"))
