#
# Run all scripts in sequence to reproduce the data set
#
# (c) 2023 Jean-Olivier Irisson, GNU General Public License v3

# Last run on:
# 2024-01-28 00:31

run_file <- function(f) {
  # list environment content
  vars <- ls(envir=.GlobalEnv)
  # run code
  message("\n", basename(f))
  source(f)
  # cleanup environment
  new_vars <- setdiff(ls(envir=.GlobalEnv), vars)
  rm(list=new_vars, envir=.GlobalEnv)

  return(invisible(TRUE))
}

run_file("1.list_projects.R")
run_file("2.get_samples.R")
run_file("3.get_volumes.R")
run_file("4.get_objects.R")
run_file("5.reformat_objects.R")
run_file("6.regroup_taxa.R")
run_file("7.prepare_contact_with_authors.R")
run_file("8.get_images.R")
run_file("9.finalise_dataset.R")
run_file("10.plot.R")
