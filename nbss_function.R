#' Normalised Biomass Size Spectrum
#'
#' @param x vector of biomasses, biovolumes, or lengths.
#' @param w vector of weights, typically concentrations associated with individual measurements in `x`.
#' @param type whether to compute a biomass/biovolume or abundance spectrum.
#' @param base base of the logarithm for the computation of bins.
#' @param binwidth width of bins in log10 scale.
#'
#' @return a data.frame with columns
#' - `bin_log` value of the bin center in log scale;
#' - `bin` value of the bin center in original scale;
#' - `binwidth` width of the bin center in original scale;
#' - `y` sum of biomass/biovolume for a biomass spectrum; count of the objects in the bin for an abundance spectrum;
#' - `norm_y` `y/binwidth`
#'
#' @export
#'
#' @examples
#' # Biovolume spectrum
#' ss <- nbss(uvp$volume_mm3)
#' head(ss)
#' autoplot(ss) + labs(
#'   x=expression("Biovolume (mm"^3*")"),
#'   y="Normalised biovolume"
#' )
#' # Abundance spectrum
#' ss <- nbss(uvp$length_mm, binwidth=0.05)
#' autoplot(ss) + labs(x="Length (mm)")
nbss_function <- function(x, w=rep(1, length(x)), type=c("biomass", "abundance"), base=10, binwidth=0.1) {

  # check arguments
  type <- match.arg(type)
  if (! base %in% c(2,10)) {
    stop("`base` must be either 2 for natural logarithm or 10 for base 10 logarithm")
  }
  if (base == 2) {
    l <- log
    il <- exp
  } else {
    l <- log10
    il <- function(x) {10^x}
  }
  if (length(x) != length(w)) {
    stop("x and w must be the same length")
  }

  # Bins from ecotaxa
  ecotaxa_bin_min <- c(0.5120,0.6450,0.8130
                       ,1.0200,1.2900,1.6300,2.0500,2.5800,3.2500,4.1000,5.1600,6.5000,8.1900
                       ,10.3000,13.0000,16.4000,20.6000)
  ecotaxa_bin_max <- c(0.6450,0.8130
                       ,1.0200,1.2900,1.6300,2.0500,2.5800,3.2500,4.1000,5.1600,6.5000,8.1900
                       ,10.3000,13.0000,16.4000,20.6000,26.0000)
  # 1 µm, 1.26 µm, 1.59 µm, 2 µm, 2.52 µm, 3.17 µm, 4 µm, 5.04 µm, 6.35 µm, 8 µm, 10.1 µm,
  # 12.7 µm, 16 µm, 20.2 µm, 25.4 µm, 32 µm, 40.3 µm, 50.8 µm,  64 µm, 80.6 µm, 102 µm, 128 µm,
  # 161 µm, 203 µm, 256 µm, 323 µm, 406 µm, 512 µm, 645 µm, 813 µm, 1.02 mm, 1.29 mm, 1.63 mm,
  # 2.05 mm, 2.58 mm, 3.25 mm, 4.1 mm, 5.16 mm, 6.5 mm, 8.19 mm, 10.3 mm, 13 mm, 16.4 mm, 20.6 mm,
  # 26 mm

  Ecotaxa_min <- data_frame(bin_min = log10(ecotaxa_bin_min))
  Ecotaxa_max <- data_frame(bin_max =  log10(ecotaxa_bin_max))
  Ecotaxa_bins = cbind(Ecotaxa_min, Ecotaxa_max)
  Ecotaxa_bins <- Ecotaxa_bins %>%
    mutate(binwidth = bin_max - bin_min,
           bin_log = bin_max -  binwidth/2,
           bin = 10^(bin_log),
           y = NA)

  for (n in 1:nrow(Ecotaxa_bins)) {
    temp <- data.frame(esd = x) %>%
      mutate(esd_log = log10(esd)) %>%
      filter(between(esd_log,Ecotaxa_bins$bin_min[n], Ecotaxa_bins$bin_max[n])) %>%
      summarise(nb = n())
    Ecotaxa_bins$y[[n]] = temp$nb
  }

  Ecotaxa_bins$norm_y <- Ecotaxa_bins$y / Ecotaxa_bins$binwidth

  # reorder columns
  ss <- Ecotaxa_bins[,c("bin_log", "bin", "binwidth", "y", "norm_y","bin_min", "bin_max")]

  # prepare output
  attr(ss, "type") <- type
  attr(ss, "base") <- base
  class(ss) <- c("nbss", class(ss))

  return(ss)
}

