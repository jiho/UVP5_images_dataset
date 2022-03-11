#
# Get metadata for all objects in selected UVP5 casts
#
# (c) 2020 Jean-Olivier Irisson, GNU General Public License v3

# NB: some bits are long (~20 min), run as a job

source("0.setup.R")
library("feather")
library("furrr")
library("ecotaxar")

# read selected samples
samples <- read_tsv("data/UVP5_samples_selected.tsv", col_types=cols_only(projid="d", sampleid="d"))

# work in parallel but do not use too many cores,
# otherwise the EcoTaxa server will be the bottleneck
# plan(multisession, workers=5)
# fall back on sequential processing
plan(sequential)
# NB: this actually seems faster!

# deal with the data project by project, because the mapping is per project
pids <- samples$projid %>% unique() %>% sort() %>% as.integer()

# get records for validated objects from selected profiles
system.time(
future_walk(pids, function(pid) {
  # determine output file name
  out_file <- file.path(proj_dir, str_c(pid, ".feather"))

  if (!file.exists(out_file)) {
    # get selected samples in the current project
    sids <- filter(samples, projid == pid) %>% pull(sampleid) %>% as.integer()

    localdb <- db_connect_ecotaxa()

    # get current project and its mapping
    prj <- tbl(localdb, "projects") %>% filter(projid==pid) %>% collect()
    mapping <- parse_mapping(prj$mappingobj)
    # remove useless or wrong fields
    mapping <- mapping[! names(mapping) %in% c("x", "y", "xm", "ym", "xstart", "ystart", "bx", "by", "esd")]

    # extract validated objects from the select samples
    obj <- tbl(localdb, "objects") %>%
      filter(projid == pid, sampleid %in% sids, classif_qual %in% c("V", "D")) %>%
      # keep relevant metadata
      select(
        # identifiers
        projid, sampleid, objid, origid=orig_id,
        # classification
        classif_id, classif_who, classif_when,
        # depth
        depth=depth_min,
        # zooprocess features (and other imported fields)
        all_of(mapping)
      ) %>%
      # add path to image within EcoTaxa's vault
      left_join(tbl(localdb, "images") %>% select(objid, imgrank, file_name), by="objid") %>%
      filter(imgrank==0L) %>%
      collect()

    people <- tbl(localdb, "objects") %>%
      # redo the original query to get the objids
      filter(projid == pid, sampleid %in% sids, classif_qual %in% c("V", "D")) %>%
      select(projid, objid, classif_who) %>%
      # use them to fetch them from the history table
      left_join(
        tbl(localdb, "objectsclassifhisto") %>% select(objid, classif_who),
        by="objid", suffix=c(".current", ".before")
      ) %>%
      collect() %>%
      # put all user ids in the same column,
      # no matter if they are the current annotator or a previous one
      pivot_longer(cols=starts_with("classif"), names_to="where", values_to="userid") %>%
      # remove classifications from a model (usersid is NA)
      filter(!is.na(userid)) %>%
      # add user-understandable identification
      left_join(tbl(localdb, "users") %>% select(userid=id, email, name), by="userid", copy=TRUE) %>%
      # compute totals per counter
      count(projid, userid, email, name)
    # TODO store this efficiently with the rest

    # write an information message
    message(
      "projid = ", format(pid, width=4, justify="right"),
      " contains ", format(length(sids), width=3, justify="right"), " samples",
      " and ", format(nrow(obj), width=8, justify="right", big.mark=","), " objects"
    )

    # save as a file
    write_feather(obj, path=out_file)

    db_disconnect_ecotaxa(localdb)
  }
}, .options=furrr_options(seed=NULL))
)
