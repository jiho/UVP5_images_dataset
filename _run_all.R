#
# Run all scripts in sequence to reproduce the data set
#
# (c) 2023 Jean-Olivier Irisson, GNU General Public License v3

# 2024-01-03 23:23
source("1.list_projects.R")
source("2.get_samples.R")
source("3.get_volumes.R")
source("4.get_objects.R")
source("5.reformat_objects.R")
source("6.regroup_taxa.R")
source("7.prepare_contact_with_authors.R")
