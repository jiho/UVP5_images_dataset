#
# Extract a subset of images to check on EcoTaxa
#
# (c) 2024 Jean-Olivier Irisson, GNU General Public License v3

o <- read_tsv("data/final/objects.tsv.gz") %>%
  select(-mid_depth_bin, -taxon, -lineage, -group_lineage)

# sub-sample some images randomly
set.seed(1)
o_liv <- o %>%
  filter(group != "Not plankton" & ! str_detect(group, "to_")) %>%
  group_by(group) %>%
  sample_n(size=min(200, n())) %>%
  ungroup()

o_not <- o %>%
  filter(group=="Not plankton") %>%
  sample_n(size=10000)

os <- bind_rows(o_liv, o_not)

# add sample information
smp <- read_tsv("data/final/samples.tsv.gz", show_col_types=FALSE)
os <- os %>%
  left_join(select(smp, sample_id, sample_uvp_model=uvp_model, object_lat=lat, object_lon=lon, datetime), by="sample_id") %>%
  mutate(object_date=format(datetime, "%Y%m%d"), object_time=format(datetime, "%H%M%S")) %>%
  select(-datetime)

# check and list groups
count(os, group) %>% print(n=100)

# remap names to be EcoTaxa compatible
taxo <- tribble(~group, ~taxon,
"Acantharea"                         , "Acantharea" ,
"Actinopterygii"                     , "Actinopterygii" ,
"Annelida"                           , "Annelida" ,
"Appendicularia"                     , "Appendicularia" ,
"Bacillariophyta (contextual)"       , "contextual<Bacillariophyta" ,
"Cephalopoda"                        , "Cephalopoda" ,
"Chaetognatha"                       , "Chaetognatha" ,
"Copepoda"                           , "Copepoda<Maxillopoda" ,
"Ctenophora"                         , "Ctenophora<Metazoa" ,
"Doliolida"                          , "Doliolida" ,
"Eumalacostraca"                     , "Eumalacostraca" ,
"Foraminifera"                       , "Foraminifera" ,
"Gymnosomata"                        , "Gymnosomata" ,
"Limacinidae"                        , "Limacinidae" ,
"Narcomedusae"                       , "Narcomedusae" ,
"Nostocales"                         , "Nostocales" ,
"Not plankton"                       , "not-living" ,
"Ostracoda"                          , "Ostracoda" ,
"Phaeodaria"                         , "Phaeodaria" ,
"Pyrosoma"                           , "Pyrosoma" ,
"Salpida"                            , "Salpida" ,
"Siphonophorae"                      , "Siphonophorae" ,
"Thecosomata (Cavolinia or Creseis)" , "Thecosomata" ,
"Trachymedusae"                      , "Trachymedusae" ,
"Trichodesmium"                      , "Trichodesmium" ,
"Trichodesmium (contextual)"         , "contextual<Trichodesmium" ,
"colonial Collodaria"                , "colonial<Collodaria" ,
"other Cnidaria"                     , "Cnidaria<Hydrozoa" ,
"other Collodaria"                   , "Collodaria" ,
"other Crustacea"                    , "Crustacea" ,
"other Hydrozoa"                     , "Hydrozoa" ,
"other Mollusca"                     , "Mollusca" ,
"other Rhizaria"                     , "Rhizaria" ,
"other living"                       , "other<living" ,
"possibly plankton"                  , "othertocheck" ,
"tentacle of Cnidaria"	              , "tentacle<Cnidaria" ,
)
any(duplicated(taxo$taxon))
# -> OK, we are not bunching several groups together

# check that the taxa names exist
db <- db_connect_ecotaxa()
eco_taxo <- tbl(db, "taxonomy") %>% select(display_name) %>% collect()
filter(taxo, ! taxon %in% eco_taxo$display_name)
# -> OK

# format .tsv for EcoTaxa
duplicated(os$object_name) %>% sum()
# -> OK, we can use object name as the unique identifier
o_eco <- os %>%
  left_join(taxo, by="group") %>%
  mutate(
    img_file_name=str_c(object_id, ".jpg"),
    img_rank=0,
    object_annotation_status=ifelse(taxon=="not-living", "predicted", "dubious")
  ) %>%
  select(
    img_file_name, img_rank,
    object_id=object_name,
    object_annotation_category=taxon, object_annotation_status,
    object_depth_min=depth, object_depth_max=depth,
    object_lat:object_time,
    area_mm2:skeleton_area,
    starts_with("sample")
  ) %>%
  rename_with(.fn=~str_c("object_", .), .cols=area_mm2:skeleton_area)

# format the output with a types rows
tsv <- format_tsv(o_eco) %>% str_split_1("\n")
types <- str_c("[", c("t", "f", "t", "t", "t", rep("f", times=ncol(o_eco)-5)), "]") %>% str_flatten(collapse="\t")
# NB: we assign [f] to sample_id and sample_uvp_model, which is wrong, but actually ignored by EcoTaxa
tsv <- append(tsv, types, after=1)

# prepare storage
eco_dir <- file.path(data_dir, "ecotaxa_import")
unlink(eco_dir, recursive=TRUE)
dir.create(eco_dir)

# write to file
eco_file <- file.path(eco_dir, "ecotaxa_uvp5.tsv")
cat(tsv, file=eco_file, sep="\n")

# copy images
imgs <- str_c(data_dir, "/images/", os$sample_id, "/", os$object_id, ".jpg")
ok <- file.copy(imgs, eco_dir)
all(ok)

# zip the result
unlink(file.path(data_dir, "ecotaxa_import.zip"))
system(str_c("cd ", data_dir, "; zip -rq ecotaxa_import.zip ecotaxa_import"))

# import on EcoTaxa and add the categories
#
