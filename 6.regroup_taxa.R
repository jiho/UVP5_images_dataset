#
# Regroup taxa into consistently sorted and mutually exclusive groups
#
# (c) 2020 Jean-Olivier Irisson, GNU General Public License v3
# (c) 2022 Julie COUSTENOBLE,

source("0.setup.R")
library("feather")

# read all objects information
o <- read_feather(file.path(data_dir, "all.feather"))

# read the taxonomic grouping
g <- read_csv("https://docs.google.com/spreadsheets/d/1a-W6dSxm9q-2-ANRsfTu6yXK3BeKDwO16kaw0NMSc9k/export?format=csv", col_types=cols()) %>%
  # TODO replace by a static .csv file once this is stabilised
  rename(group=group_laeti)

# Keep only used cols
g <- subset(g, select = c(lineage, taxon, nb_objects, group))

# Inspect the totals
totals_by_groups <- g %>% group_by(group) %>% summarise(tot=sum(nb_objects)) %>% ungroup() %>% arrange(desc(tot))
#    group              tot
#    <chr>            <dbl>
# 1 detritus            6410233
# 2 artefact            1297217
# 3 Copepoda            174404
# 4 Trichodesmium       98990
# 5 Phaeodaria          71974
# 6 to_resort           65707
# 7 turbid              24964
# 8 to_rename           18293
# 9 Acantharea          13430
# 10 Eumalacostraca     13273
# 11 bubble             9448
# 12 Hydrozoa_others    8661
# 13 Collodaria_others  7953
# 14 Ostracoda          7880
# 15 misc               7760
# -> this is not super satisfying: the third most abundant biological group is ... to_rename + to_resort !

# Try to infer a lineage for the new grouping
# get all groups
groups <- distinct(g, group) #36 groups
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

#TODO ask to JO
group_lineages$lineage[which(group_lineages$group_for_match=="Collodaria_colonial")] <- "??/Collodaria_colonial"
group_lineages$lineage[which(group_lineages$group_for_match=="to_rename")] <- "??/to_rename"
group_lineages$lineage[which(group_lineages$group_for_match=="to_resort")] <- "??/to_resort"

# add lineage to the grouping
g <- left_join(g, group_lineages, by=c("group"))

# and add to the objects file
o_g <- o %>%
  inner_join(select(g, taxon, group, lineage.y))

names(o_g)[names(o_g) == 'lineage.y'] <- 'group_lineage'

# write to disk
write_feather(o_g, file.path(data_dir, "all.feather"))
