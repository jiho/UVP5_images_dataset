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
  arrange(pprojid)

# read the list of currently selected projects
current_projects_list <- read_csv("https://docs.google.com/spreadsheets/d/1CrR-5PdhQ09JSTU482HOkTjtCvXRmpngYDuYSwaGQAo/export?format=csv", col_types=cols())

# check that no currently selected project disappeared
disappeared <- filter(current_projects_list, use != "" & ! pprojid %in% projects$pprojid) %>% print()
# -> None OK

# get the ones that are used and add them to the current list
projects <- left_join(projects, select(current_projects_list, pprojid, use), by="pprojid")

# write it into a file
write_tsv(projects, "data/UVP5_projects.tsv", na="")
# now copy paste this in https://docs.google.com/spreadsheets/d/1CrR-5PdhQ09JSTU482HOkTjtCvXRmpngYDuYSwaGQAo/ and possibly select additional projects to use in the global dataset
