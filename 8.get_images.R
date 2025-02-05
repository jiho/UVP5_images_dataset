#
# Get image files
#
# (c) 2020 Jean-Olivier Irisson, GNU General Public License v3

source("0.setup.R")
library("future")
library("furrr")

# avoid scientific notation so that filenames for object_ids which are large ints are printed correctly
# e.g. 4000000 instead of 4e+06
options(scipen=999)

# read relevant pieces of the data table
o <- read_feather(file.path(data_dir, "all.feather"), col_select=c("projid", "sampleid", "objid", "file_name"))


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
message(nrow(o), " objects")

# list images already copied
copied <- system2("find", args=str_c(img_dir, " -type f"), stdout=TRUE)
message(length(copied), " images files already present")
# and remove them from the list
o_to_copy <- filter(o, ! dest %in% copied)
message(length(o_to_copy), " images to copy")


# copy the remaining images, in parallel
plan(multisession, workers=5)
o_to_copy %>% split(o_to_copy$sampleid) %>% future_walk(~file.copy(.$source, .$dest))
# close the parallel workers
plan(sequential)

# remove images that should not be there
copied <- system2("find", args=str_c(img_dir, " -type f"), stdout=TRUE)
extra_imgs <- copied[!copied %in% o$dest]
message(length(extra_imgs), " extra images to remove")
if (length(extra_imgs) > 0 ) {
  unlink(extra_imgs)
}

# check that everything has been copied
missing_imgs <- filter(o, ! dest %in% copied)
message(nrow(missing_imgs), " missing images")

# TODO reformat all images to cut the 31 px at the bottom and keep only the largest object?
# TODO measure the new images with scikit image?
