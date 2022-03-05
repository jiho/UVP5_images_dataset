#
# Perform common actions for all scripts
#
# (c) 2020 Jean-Olivier Irisson, GNU General Public License v3

# load packages
suppressMessages(library("tidyverse"))
suppressMessages(library("RPostgreSQL"))

# connect to databases
# ecoTaxa
dbt <- RPostgreSQL::dbConnect("PostgreSQL", host="ecotaxa.obs-vlfr.fr", dbname="ecotaxa", user="zoo", password="z004ecot@x@")
# ecoPart
dbp <- RPostgreSQL::dbConnect("PostgreSQL", host="ecotaxa.obs-vlfr.fr", dbname="ecopart", user="zoo", password="zoo12", port=5435)

# create directories for large data that should live outside the repository
# NB: no trailing /
data_dir <- "~/datasets/UVP5_images_dataset"
proj_dir <- "~/datasets/UVP5_images_dataset/projects"
img_dir <- "~/datasets/UVP5_images_dataset/images"

dir.create(data_dir, showWarnings=FALSE, recursive=TRUE)
dir.create(proj_dir, showWarnings=FALSE, recursive=TRUE)
dir.create(img_dir, showWarnings=FALSE, recursive=TRUE)
