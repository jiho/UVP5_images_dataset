# (c) 2024 Jean-Olivier Irisson, GNU General Public License v3

#' Compute concentration, biovolume, and average grey level per profile, taxonomic group and depth bin
#'
#' @param obj data.frame with sample_id, group, depth, vol_mm3, and mean (mean grey)
#' @param vol data.frame with sample_id, mid_depth_bin, water_volume_imaged
#' @param depth_breaks vector of depth values delimiting bins
#'
#' @returns A data.frame with sample_id, depth_bin, group, concentration, biovolume.
properties_per_bin <- function(obj, vol, depth_breaks=c(0,100)) {
  if (length(depth_breaks) < 2) {
    stop("Need at least two depths to define a bin")
  }

  # compute sampled volume per depth bin
  vol_per_bin <- vol %>%
    # cut into depth bins
    dplyr::mutate(depth_bin=cut(mid_depth_bin, breaks=depth_breaks, include.lowest=TRUE, dig.lab=5)) %>%
    # compute total volume imaged in these new bins
    dplyr::group_by(sample_id, depth_bin) %>%
    dplyr::summarise(water_volume_imaged=sum(water_volume_imaged), max_depth=max(mid_depth_bin)+2.5, .groups="drop") %>%
    # remove incompletely sampled bins
    dplyr::mutate(complete=stringr::str_detect(as.character(depth_bin), str_c(max_depth, "]"))) %>%
    dplyr::filter(complete) %>%
    dplyr::select(-max_depth, -complete)

  # compute concentration and biovolume per depth bin
  res <- obj %>%
    # cut into the same depth bins as above
    dplyr::mutate(depth_bin=cut(depth, breaks=depth_breaks, include.lowest=TRUE, dig.lab=5)) %>%
    # force the taxonomic group to be a factor
    # (this ensures that every group is considered at every depth bin of every profile)
    dplyr::mutate(group=factor(group)) %>%
    # for each depth bin of each profile, for each group (including those absent)
    dplyr::group_by(sample_id, depth_bin, group, .drop=FALSE) %>%
    # compute abundance and total volume
    dplyr::summarise(n=n(), vol=sum(vol_mm3), avg_grey=mean(mean), .groups="drop") %>%
    # add water volume sampled
    dplyr::inner_join(vol_per_bin) %>% # NB: using inner_join reduces to completely sampled bins
    # and compute concentrations and biovolumes
    dplyr::mutate(
      concentration=n/water_volume_imaged,
      biovolume=vol/water_volume_imaged,
    ) %>%
    # re-order, subset and rename columns
    dplyr::select(sample_id, depth_bin, group, concentration, biovolume, avg_grey)

  return(res)
}

