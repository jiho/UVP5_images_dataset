#
# Compare the current list of profiles with those used by Thelma and Laetitia
#
# (c) 2020 Jean-Olivier Irisson, GNU General Public License v3

source("0.setup.R")

# read the samples used by Thelma
thelma_samples <- read_csv("data/list_profiles_ecopart-thelma.csv") %>%
  select(title=ecotaxa, ptitle=ecopart, profile=profiles) %>%
  separate_rows(profile, sep=",")

# get our current selection of samples
current_samples <- read_tsv("data/UVP5_samples.tsv", col_types=cols()) %>%
  select(ends_with("id"), profile) %>%
  # add projects titles
  left_join(
    read_tsv("data/UVP5_projects.tsv", col_types=cols()) %>%
      select(pprojid, projid, ptitle, title)
  )

# Show a list of profiles per project
#
# @param x data.frame of projects + profiles as above
# @param title_pattern show only the projects matching the pattern (by default shows everything)
show_profiles <- function(x, title_pattern=".") {
  filter(x, str_detect(str_to_lower(title), title_pattern)) %>%
    split(.$title) %>%
    walk(function(piece){
      message(piece$title[1], " [", piece$projid[1], "]")
      print(piece$profile)
    })
}

# profiles in Thelma's selection but not in ours
filter(thelma_samples, ! profile %in% current_samples$profile) %>% show_profiles()

# profiles in our selection but not in Thelma's
filter(current_samples, ! profile %in% thelma_samples$profile) %>% show_profiles()
