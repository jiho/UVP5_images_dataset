#
# Perform common actions for all scripts
#
# (c) 2020 Jean-Olivier Irisson, GNU General Public License v3

# load packages
suppressMessages(library("tidyverse"))
suppressMessages(library("DBI"))
suppressMessages(library("ecotaxar"))

# connect to databases
dbt <- db_connect_ecotaxa()
dbp <- db_connect_ecopart()

# create directories for large data that should live outside the repository
# NB: no trailing /
data_dir <- "~/datasets/UVP5_images_dataset"
proj_dir <- "~/datasets/UVP5_images_dataset/projects"
img_dir <- "~/datasets/UVP5_images_dataset/images"

dir.create(data_dir, showWarnings=FALSE, recursive=TRUE)
dir.create(proj_dir, showWarnings=FALSE, recursive=TRUE)
dir.create(img_dir, showWarnings=FALSE, recursive=TRUE)
