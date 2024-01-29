#
# Prepare emails for data owners regarding
# - what is left to annotate
# - who did what
#
# (c) 2024 Jean-Olivier Irisson, GNU General Public License v3

source("0.setup.R")

## Read data ----

# object level info
o <- read_feather(file.path(data_dir, "all.feather"), col_select=c("projid", "annotators", "lineage", "taxon", "group_lineage", "group"))

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
  left_join(select(proj, projid, data_owner), by="projid") %>%
  unnest(cols=annotators) %>%
  count(data_owner, annotators)

annots <- annot_counts %>%
  # add project titles
  left_join(
    proj %>%
      group_by(data_owner) %>%
      summarise(projects = str_c(str_c(title, " <http://ecotaxa.obs-vlfr.fr/prj/", projid, ">"), collapse=" ; ")),
    by="data_owner"
  ) %>%
  # reorganise data
  select(data_owner, projects, annotators, n) %>%
  arrange(data_owner, desc(n)) %>%
  # remove repeated elements
  mutate(data_owner=unfill(data_owner), projects=unfill(projects))

write_tsv(annots, "data/authors.tsv", na="")

# TODO fill with what Julie already collected


## Extract information to help re-sorting ----

g <- read_csv("https://docs.google.com/spreadsheets/d/1NFgpzkFXVBEuobaggmApIYgQ2HE837zwsStu7WTU5hQ/export?format=csv", show_col_types=FALSE)

# compute some stats to get an idea how important resorting would be
stats <- count(o, group) %>%
  arrange(desc(n))
# compute percentage of total and among living groups
not_liv_groups <- c("Not plankton")
tot_liv <- stats %>%
  filter(!group %in% not_liv_groups) %>%
  pluck("n") %>% sum()
stats <- stats %>%
  mutate(
    perc=n/sum(n)*100,
    perc_liv=n/tot_liv * 100
  )
# print(stats, n=100)

# relevant stats
filter(stats, group=="Not plankton")
# -> 92% of not living

stats %>%
  filter(group %in% str_subset(stats$group, "to_")) %>%
  pluck("perc_liv") %>% sum()
# -> ~5% of living stuff that still need some actions

filter(stats, group=="possibly plankton")
# -> <1% of the living is in a group we're not sure about

# prepare emails regarding what to do
taxa_to_review <- filter(g, !is.na(comment_final))$taxon %>% unique()

# extract count of objects to review per project
to_review <- o %>%
  select(projid, lineage, taxon, group_lineage, group) %>%
  filter(taxon %in% taxa_to_review) %>%
  count(projid, taxon)

# add other relevant info
to_review <- to_review %>%
  left_join(select(g, taxon, taxo_id, comment=comment_final), by="taxon") %>%
  left_join(select(proj, projid, title, data_owner), by="projid")

# prepare emails
cat("", file="data/emails.txt")
to_review %>%
  group_by(data_owner) %>% group_walk(function(.x, .y) {

    mess_per_proj <- .x %>% group_by(projid) %>% group_map(function(.x, .y) {
      x <- bind_cols(.x, .y)
      str_c(
        # title
        str_glue_data(x[1,], "## Project {title} : https://ecotaxa.obs-vlfr.fr/prj/{projid}"),
        "\n\n",
        # all taxa to review
        str_flatten(
          str_glue_data(x, "review '{taxon}' [n={n}], https://ecotaxa.obs-vlfr.fr/prj/{projid}?taxo={taxo_id}\n  ↳  {comment}", .na=""),
          collapse="\n"
        )
      )
    })

    mess <- str_c(
      # define owner
      "\n# ", .y$data_owner, "\n\n",
      # print all projects to review, for this owner
      str_flatten(mess_per_proj, collapse="\n\n\n"),
      # add separator
      "\n\n----\n\n"
    )
    cat(mess, file="data/emails.txt", append=TRUE)
  })
