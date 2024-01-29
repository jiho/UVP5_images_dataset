#
# Perform common actions for all scripts
#
# (c) 2020 Jean-Olivier Irisson, GNU General Public License v3

# load packages
suppressMessages(library("tidyverse"))
suppressMessages(library("DBI"))
suppressMessages(library("ecotaxar"))
suppressMessages(library("arrow"))

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

# useful function to write concise output
#' Replace similar consecutive elements by NA
#'
#' Has methods for vectors and data.frames
#' = opposite of tidyr::fill()
#' useful before writing to disk with write_csv(unfill(x), "somefile.csv", na="")
#' @examples
#' df <- data.frame(style=c("A", "A", "B", "B", "B"), value=runif(5))
#' unfill(df$style)
#' unfill(df)
unfill <- function(x, ...) {
  UseMethod("unfill")
}
unfill.default <- function(x, ...) {
  same <- x == dplyr::lag(x)
  ifelse(!is.na(same) & same, NA, x)
}
unfill.data.frame <- function(x, ...) {
  mutate_all(x, unfill.default)
}
