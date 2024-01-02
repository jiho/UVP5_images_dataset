#
# Prepare emails for data owners regarding
# - what is left to annotate
# - who did what
#
# (c) 2024 Jean-Olivier Irisson, GNU General Public License v3

source("0.setup.R")
library("arrow")

## Read data ----

# object level info
o <- read_feather(file.path(data_dir, "all.feather"))

# projects
proj <- read_tsv("data/UVP5_projects_selected.tsv", show_col_types=FALSE) %>%
  filter(use != "") %>%
  select(data_owner:license) %>%
  distinct()

# define actual data owners for each EcoTaxa project
# NB: the data owner is defined on EcoPart's side and one EcoPart project can be linked to several EcoTaxa projects
#     this is the case for MooseGE and for Dyfamed; in both cases the first owner by alphabetic order is OK
proj <- proj %>%
  arrange(projid, data_owner) %>%
  group_by(projid) %>%
  slice_head(n=1) %>%
  ungroup() %>%
  # cleanup some data owners
  mutate(data_owner=data_owner %>% str_replace("rainer.kiko@obs-vlfr.fr", "rkiko@geomar.de" ) %>% str_replace("^stemmann@obs-vlfr.fr", "lars.stemmann@obs-vlfr.fr"))

proj$data_owner %>% unique() %>% sort()


## Prepare authors list ----

# count annotation effort per person and project
annot_counts <- o %>%
  select(projid, annotators) %>%
  left_join(select(proj, projid, data_owner)) %>%
  unnest(cols=annotators) %>%
  count(data_owner, annotators)

annots <- annot_counts %>%
  # add project titles
  left_join(
    proj %>%
      group_by(data_owner) %>%
      summarise(projects = str_c(str_c(title, " <http://ecotaxa.obs-vlfr.fr/prj/", projid, ">"), collapse=" ; "))
  ) %>%
  # reorganise data
  select(data_owner, projects, annotators, n) %>%
  arrange(data_owner, desc(n)) %>%
  # remove repeated elements
  mutate(data_owner=unfill(data_owner), projects=unfill(projects))

write_tsv(annots, "data/authors.tsv", na="")


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
