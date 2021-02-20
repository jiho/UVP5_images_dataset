#
# Regroup taxa into consistently sorted and mutually exclusive groups
#
# (c) 2020 Jean-Olivier Irisson, GNU General Public License v3

source("0.setup.R")
library("feather")

# read all objects information
o <- read_feather(file.path(data_dir, "all.feather"))

# read the taxonomic grouping
g <- read_csv("https://docs.google.com/spreadsheets/d/1NFgpzkFXVBEuobaggmApIYgQ2HE837zwsStu7WTU5hQ/export?format=csv", col_types=cols()) %>%
  # TODO replace by a static .csv file once this is stabilised
  rename(group=group_thelma) %>%
  # remove some taxa = those that are not renamed
  drop_na(group) %>%
  # remove taxa were are unsure off
  filter(!str_detect(group, "\\?")) %>%
  # TODO remove this once the list is complete
  select(taxon:group)

# Inspect the totals
g %>% group_by(group) %>% summarise(tot=sum(n)) %>% ungroup() %>% arrange(desc(tot))
#    group                           tot
#    <chr>                         <dbl>
#  1 detritus                    5757083
#  2 artefact                    1306383
#  3 Copepoda                     164835
#  4 Trichodesmium                 90737
#  5 misc                          66529
#  6 Phaeodaria                    62905
#  7 Acantharea                    12559
#  8 Eumalacostraca                11141
#  9 Collodaria                     9368
# 10 Appendicularia                 8900
# 11 bubble                         8644
# 12 Ostracoda                      8511
# 13 Hydrozoa_others                7451
# 14 Chaetognatha                   5400
# 15 Crustacea_others               4579
# -> this is not super satisfying: the third most abundant biological group is ... misc!

# Try to infer a lineage for the new grouping
# get all groups
groups <- distinct(g, group)
# get the available taxonomy
taxo <- distinct(o, lineage, taxon)
# start with perfect matches
group_lineages <- groups %>%
  mutate(
    group_for_match=group %>%
      str_replace("_others", "") %>%
      str_replace("_cavo_or_creseis", "")
  ) %>%
  left_join(taxo, by=c("group_for_match"="taxon"))
# and fill the rest manually
group_lineages$lineage[which(group_lineages$group_for_match=="Nostocales")] <- "living/Bacteria/Proteobacteria/Cyanobacteria/Nostocales"
# filter(taxo, str_detect(lineage, "Nostocales"))
group_lineages$lineage[which(group_lineages$group_for_match=="Cnidaria")] <- "living/Eukaryota/Opisthokonta/Holozoa/Metazoa/Cnidaria"
# filter(taxo, str_detect(lineage, "Cnidaria"))
group_lineages$lineage[which(group_lineages$group_for_match=="misc")] <- "living"
group_lineages


# add lineage to the grouping
g <- left_join(g, group_lineages)

# and add to the objects file
# use an inner_join to remove the objects which belonged to groups that are dropped (i.e. not renamed)
o_g <- o %>%
  inner_join(select(g, taxon, group, group_lineage=lineage))

nrow(o) - nrow(o_g)
# -> we're removing 40k rows
#    this is mostly duplicates (25k) and darksphere (19k)

# write to disk
write_feather(o_g, file.path(data_dir, "all.feather"))
