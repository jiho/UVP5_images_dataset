#
# Run all scripts in sequence to reproduce the data set
#
# (c) 2023 Jean-Olivier Irisson, GNU General Public License v3

# 2024-01-28 00:31
message("\nList projects")
source("1.list_projects.R")
message("\nGet samples")
source("2.get_samples.R")
message("\nGet volumes")
source("3.get_volumes.R")
message("\nGet objects")
source("4.get_objects.R")
message("\nReformat objects")
source("5.reformat_objects.R")
message("\nRegroup taxa")
source("6.regroup_taxa.R")
message("\nPrepare email for authors")
source("7.prepare_contact_with_authors.R")
message("\nGet images")
source("8.get_images.R")
message("\nFinalise dataset")
source("9.finalise_dataset.R")
message("\nProduce plots")
source("10.plot.R")
