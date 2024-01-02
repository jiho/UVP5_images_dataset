#
# Prepare emails for data owners regarding
# - what is left to annotate
# - who did what
#
# (c) 2024 Jean-Olivier Irisson, GNU General Public License v3


proj <- read_tsv("data/UVP5_projects_selected.tsv", show_col_types=FALSE) %>%
  select(pprojid:license)

## Prepare authors list ----

oc %>%
  select(projid, annotators) %>%
  unnest(data=annotators)

## Extract information to help re-sorting ----

# list
# t0* and othertocheck

filter(oc, str_detect(lineage, "Dinophyceae")) %>% count(projid)
filter(oc, taxon == "Harosa") %>% count(projid)
filter(oc, taxon == "Rhizaria X") %>% count(projid)
filter(oc, taxon == "rhizaria like") %>% count(projid)
filter(oc, taxon == "solitaryblack-like") %>% count(projid)
filter(oc, taxon == "Arthropoda") %>% count(projid)
filter(oc, str_detect(taxon, "Tunicata")) %>% count(projid)
filter(oc, str_detect(taxon, "Enteropneusta")) %>% count(projid)
filter(oc, taxon == "darksphere") %>% count(projid)
filter(oc, str_detect(taxon, "t0")) %>% count(taxon, projid) %>% filter(n>500)
