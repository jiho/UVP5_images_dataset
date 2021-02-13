#
# Get image files
#
# (c) 2020 Jean-Olivier Irisson, GNU General Public License v3

source("0.setup.R")
library("feather")
library("fs")
library("furrr")

# read relevant pieces of the data table
o <- read_feather(path(data_dir, "all.feather"), columns=c("projid", "sampleid", "objid", "file_name"))


## Copy all files to the images directory ----

# homogenize the path to img_dir (transform ~ into /home/.../ )
img_dir <- path_expand(img_dir)

# make sub directories per sample, to avoid having too many files in a single directory
dir_create(img_dir, unique(o$sampleid))

# prepare the source and destination for each image
o <- mutate(o,
  source=path("/remote/ecotaxa/vault/", file_name),
  dest=path(img_dir, sampleid, objid, ext="jpg")
)

# list images already copied
copied <- dir_ls(img_dir, recurse=TRUE, type="file")
length(copied)
# and remove them from the list
o <- filter(o, ! dest %in% copied)

# copy the remaining images, in parallel
plan(multisession, workers=5)
o %>% split(o$sampleid) %>% future_walk(~file_copy(.$source, .$dest))
# close the parallel workers
plan(sequential)

# verify that all images have been copied
copied <- dir_ls(img_dir, recurse=TRUE, type="file")
length(copied) == nrow(o)

# TODO reformat all images to cut the 31 px at the bottom and keep only the largest object?
# TODO measure the new images with scikit image?
