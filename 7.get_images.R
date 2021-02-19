#
# Get image files
#
# (c) 2020 Jean-Olivier Irisson, GNU General Public License v3

source("0.setup.R")
library("feather")
library("furrr")

# read relevant pieces of the data table
o <- read_feather(file.path(data_dir, "all.feather"), columns=c("projid", "sampleid", "objid", "file_name"))


## Copy all files to the images directory ----

# homogenize the path to img_dir (transform ~ into /home/.../ )
img_dir <- path.expand(img_dir)

# make sub directories per sample, to avoid having too many files in a single directory
walk(file.path(img_dir, unique(o$sampleid)), dir.create, showWarnings=FALSE)

# prepare the source and destination for each image
o <- mutate(o,
  source=file.path("/remote/ecotaxa/vault", file_name),
  dest  =file.path(img_dir, sampleid, str_c(objid, ".jpg"))
)

# list images already copied
copied <- system2("find", args=str_c(img_dir, " -type f"), stdout=TRUE)
length(copied)
# and remove them from the list
o_to_copy <- filter(o, ! dest %in% copied)

# copy the remaining images, in parallel
plan(multisession, workers=5)
o_to_copy %>% split(o$sampleid) %>% future_walk(~file.copy(.$source, .$dest))
# close the parallel workers
plan(sequential)

# verify that all images have been copied
copied <- system2("find", args=str_c(img_dir, " -type f"), stdout=TRUE)
length(copied) == nrow(o)

# remove images that should not be there
extra_imgs <- copied[!copied %in% o$dest]
if (length(extra_imgs) > 0 ) {
  unlink(extra_imgs)
}

# TODO reformat all images to cut the 31 px at the bottom and keep only the largest object?
# TODO measure the new images with scikit image?
