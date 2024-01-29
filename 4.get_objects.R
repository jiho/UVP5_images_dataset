#
# Get metadata for all objects in selected UVP5 casts
#
# (c) 2020 Jean-Olivier Irisson, GNU General Public License v3

# NB: some bits are long (~20 min), run as a job

source("0.setup.R")
library("future")
library("furrr")

# read selected samples
samples <- read_tsv("data/UVP5_samples_selected.tsv", col_types=cols_only(projid="d", sampleid="d"))

# work in parallel but do not use too many cores,
# otherwise the EcoTaxa server will be the bottleneck
plan(multisession, workers=3)
# fall back on sequential processing
# plan(sequential)
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

    # get list of people who sorted these objects, including in the history
    people <- tbl(localdb, "objects") %>%
      # redo the original query to get the objids
      filter(projid == pid, sampleid %in% sids, classif_qual %in% c("V", "D")) %>%
      select(projid, objid, classif_who) %>%
      # use them to fetch previous annotators
      left_join(
        tbl(localdb, "objectsclassifhisto") %>% select(objid, classif_who_previous=classif_who),
        by="objid"
      ) %>%
      collect() %>%
      # summarise all annotations in a single column
      group_by(projid, objid) %>%
      summarise(classif_who_all=list(c(classif_who[1], na.omit(classif_who_previous))), .groups="drop")

    # add list of all anotators in the original data
    obj <- left_join(obj, people, by=join_by(projid, objid))

    # write an information message
    message(
      "projid = ", format(pid, width=4, justify="right"),
      " contains ", format(length(sids), width=3, justify="right"), " samples",
      " and ", format(nrow(obj), width=8, justify="right", big.mark=","), " objects"
    )

    # save as a file
    write_feather(obj, sink=out_file)
    db_disconnect_ecotaxa(localdb)
  }
}, .options=furrr_options(seed=NULL))
)
