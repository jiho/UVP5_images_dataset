#
# Regroup taxa into consistently sorted and mutually exclusive groups
#
# (c) 2020 Jean-Olivier Irisson, Julie COUSTENOBLE, GNU General Public License v3

source("0.setup.R")

## Perform regrouping ----

# Manually:
# - Open data/UVP5_taxo.tsv in a spreadsheet (e.g. Google Sheets)
# - Add a column which will contain the new category names, regrouped
# - Regroup categories by assigning them the same name
# - To highlight changes in taxonomic names between columns D and C:
#   Conditional formatting > Custom formula > =IF(D1<>B1,1,0)
# - To compute the final count in the regrouped categories, assuming n is column C and the new groups are in column D
#   Add a column with the formula: =SUMIF(D:D,D2,$C:$C)

# Alternatively, if the sheet already exists, use it with our new taxonomic counts:
read_tsv("data/UVP5_taxo.tsv", show_col_types=FALSE) %>%
  left_join(
    read_csv("https://docs.google.com/spreadsheets/d/1NFgpzkFXVBEuobaggmApIYgQ2HE837zwsStu7WTU5hQ/export?format=csv", col_types=cols()) %>%
    select(-taxon, -taxo_id, -n),
    by="lineage"
  ) %>%
  write_tsv("data/UVP5_taxo_base.tsv", na="")
# now copy-paste that in the Google Sheet at https://docs.google.com/spreadsheets/d/1NFgpzkFXVBEuobaggmApIYgQ2HE837zwsStu7WTU5hQ/

## Apply it to the data ----

# read the taxonomic grouping
g <- read_csv("https://docs.google.com/spreadsheets/d/1NFgpzkFXVBEuobaggmApIYgQ2HE837zwsStu7WTU5hQ/export?format=csv", show_col_types=FALSE) %>%
# TODO replace by a static .tsv file once this is stabilized
# g <- read_tsv('data/UVP5_taxo_regrouped.tsv', show_col_types=FALSE)
    rename(group=group_final)

# Keep only used cols
g <- subset(g, select = c(lineage, taxon, n, group))

# Inspect the totals
totals_by_groups <- g %>% group_by(group) %>% summarise(tot=sum(n)) %>% ungroup() %>% arrange(desc(tot))
# print(totals_by_groups)
# A tibble: 41 × 2
#   group                                  tot
#   <chr>                                <dbl>
# 1 Not plankton                       7055138
# 2 Copepoda                            173909
# 3 Trichodesmium (contextual)           98956
# 4 Trichodesmium                        88676
# 5 Artefact                             67484
# 6 Phaeodaria                           65030
# 7 Bacillariophyta (contextual)         44637
# 8 to_check                             35348
# 9 possibly plankton                    19796
# -> Quite a few to be checked still...

# Infer a lineage for the new grouping
group_lineages <- distinct(g, lineage, group) %>%
  group_by(group) %>%
  group_map(function(.x, .y) {
    # default to an empty lineage
    lin <- ""

    # if, in the lineages, one ends by the name of the group, use this one
    matching_leaf <- which(basename(.x$lineage) == .y$group)
    if (length(matching_leaf) > 0) {
      lin <- .x$lineage[matching_leaf[1]]
    }

    # otherwise, try to find a full match for the group name in the lineages
    if (lin == "") {
      matches <- str_locate(.x$lineage, .y$group)
      ends <- matches[,2]
      idx <- which.max(ends)
      max_position <- max(ends)
      if (length(idx) > 0) {
        lin <- str_sub(.x$lineage[idx], start=1, end=max_position)
      }
    }

    # otherwise, pick the longest common path among all lineages in the group
    if (lin == "") {
      min_lineage_length <- min(str_length(.x$lineage))
      common <- map_lgl(1:min_lineage_length, function(i) {
        n_unique_paths <- str_sub(.x$lineage, start=1, end=i) %>% unique() %>% length()
        n_unique_paths == 1
      })
      idx_common <- which(common)
      if (length(idx_common) > 0) {
        lin <- str_sub(.x$lineage[1], start=1, end=max(idx_common)) %>% str_remove("/$")
      }
    }

    return(data.frame(group=.y$group, group_lineage=lin))
  }) %>%
  bind_rows()
# correct some manually
group_lineages$group_lineage <- str_remove(group_lineages$group_lineage, "/t0")

# add lineage to the grouping
g <- left_join(g, group_lineages, by="group")

# read info for all objects and add the grouping
o <- read_feather(file.path(data_dir, "all.feather")) %>%
  left_join(select(g, taxon, group_lineage, group), by="taxon")
# check that we have groups for all objects
sum(is.na(o$group))
# -> OK

# write to disk
write_feather(o, file.path(data_dir, "all.feather"))
