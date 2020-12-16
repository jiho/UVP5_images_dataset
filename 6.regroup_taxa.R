#
# Regroup taxa into consistently sorted and mutually exclusive groups
#
# (c) 2020 Jean-Olivier Irisson, GNU General Public License v3

source("0.setup.R")
library("feather")

# read all objects information
o <- read_feather(file.path(data_dir, "all.feather"))

g <- read_csv("https://docs.google.com/spreadsheets/d/1cILxbd4BN3Qez0bSf2Up69Gh2-Jdbzp4aXGsZ31RMUg/export?format=csv") %>%
  drop_na(taxon)
# TODO replace by a static .csv file once this is stabilised

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
group_lineages$lineage[which(group_lineages$group_for_match=="misc?")] <- "living"
group_lineages$lineage[which(group_lineages$group_for_match=="?")] <- "living"

# add lineage to the grouping
g <- left_join(g, group_lineages)

# get a few stats
# g %>% drop_na(group) %>% group_by(lineage, group) %>% summarise(n=sum(n), .groups="drop") %>% View()

# and add to the objects file
left_join(o, select(g, taxon, group, group_lineage=lineage) %>% na.omit()) %>%
  write_feather(file.path(data_dir, "all.feather"))
