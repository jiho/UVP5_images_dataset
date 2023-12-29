#
# List EcoPart projects to determine which to use
#
# (c) 2020 Jean-Olivier Irisson, GNU General Public License v3

source("0.setup.R")

# get project info from EcoPart
part_projects <- tbl(dbp, "part_projects") %>%
  # only for UVP5 projects
  filter(instrumtype=="uvp5") %>%
  select(pprojid, ptitle, data_owner=do_email, projid) %>%
  collect()

# get project info from EcoTaxa
eco_projects <- tbl(dbt, "projects") %>%
  select(projid, title, license) %>%
  collect()

# combine the two
projects <- left_join(part_projects, eco_projects, by="projid") %>%
  arrange(pprojid) %>%
  # add empty column
  mutate(use="")

# write it into a file
write_tsv(projects, "data/UVP5_projects.tsv")
# now copy paste this in a Google Sheet and select the ones to use in the global dataset
