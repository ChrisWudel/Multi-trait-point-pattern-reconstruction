#' compute_statistics
#'
#' @description Computes various summary statistics for a given point pattern. 
#' This includes common spatial statistics such as D_k, K, H_s, and pcf based on user input.
#' The function utilizes the `spatstat` package to compute these statistics and returns them in a list.
#'
#' @param x A numeric vector containing the x-coordinates of the points from the reference point pattern.
#' @param y A numeric vector containing the y-coordinates of the points from the reference point pattern.
#' @param k A numeric vector of values k, used only if the D_k-function is included in the spatial statistics to be computed (see `w_statistics`).
#' @param xr A numeric vector of length 2 defining the x-extension of the observation window (start and end).
#' @param yr A numeric vector of length 2 defining the y-extension of the observation window (start and end).
#' @param w_statistics A named vector of spatial statistics to include in the energy calculation. 
#' This may include any of the following statistics: D_k, K, H_s, or pcf. The function computes each statistic 
#' that is specified in this vector using corresponding methods from the `spatstat` package.
#' @param bw A bandwidth parameter used for kernel density estimation or spatial smoothing, affecting some statistics (e.g., pcf).
#' @param divisor A divisor for normalization or other scaling purposes within the summary statistics calculation. This parameter is 
#' relevant for some statistics (e.g., pcf).
#' @param kernel_arg The kernel function or arguments used in kernel density estimation, relevant for functions like pcf.
#' @param r A numeric vector of radii values defining the range for which the statistics are computed, typically from r_min to r_max.
#'
#' @details
#' This function computes various spatial statistics using the `spatstat` package. The available statistics include:
#' - D_k: The k-nearest neighbor distance function.
#' - K: The pair correlation function, K(r).
#' - pcf: The pair correlation function (spherical contact distribution) using kernel smoothing.
#' - H_s: The pair correlation function using a specific estimation method from `spatstat`.
#' 
#' The statistics are computed for each of the specified values in the w_statistics vector, and the results are returned 
#' as a list.
#'
#' @return A list containing the computed spatial statistics. Each element in the list corresponds to one of the statistics
#' specified in the w_statistics parameter, with the respective function result.
#'
#' @aliases compute_statistics
#' @rdname compute_statistics
#'
#' @keywords internal 
#'
#' @examples
#' \dontrun{
#' # Example usage of compute_statistics
#' stats <- compute_statistics(x = x_coords, y = y_coords, k = k_values, 
#'                             xr = c(0, 100), yr = c(0, 100), 
#'                             w_statistics = c("Dk", "K", "pcf"), 
#'                             bw = 0.5, divisor = "r", kernel_arg = "gaussian", r = seq(0, 50, by = 5))
#' }
#' 
compute_statistics <- function(x, y, k, xr, yr, w_statistics, bw, divisor, kernel_arg, r) {

  stat <- names(w_statistics)
  names(stat) <- stat
  lapply(stat, function(name) switch(name,
    # Calculation of the Dk(r)-function, if this is to be taken into account for the energy calculation.
    Dk = {
      nnd_ <- as.matrix(spatstat.geom::nndist(x, y, k=k))
      apply(nnd_, 2, function(z) cumsum(graphics::hist(z[z <= max(r)], breaks = c(-Inf, max(r)), plot = FALSE) $ count) / length(z))
    },

    # Calculation of the K(r)-function, if this is to be taken into account for the energy calculation.
    K = {
      kest<-spatstat.explore::Kest(spatstat.geom::ppp(x,y,window=spatstat.geom::owin(xr,yr)), rmax=max(r), correction="none")
      kest$un
    },

    # Calculation of the pcf(r)-function (spherical contact distribution), if this is to be taken into account for the energy calculation.
    pcf = {
      pcfest<-spatstat.explore::pcf.ppp(spatstat.geom::ppp(x,y,window=spatstat.geom::owin(xr,yr)), r=c(0,r), kernel=kernel_arg, divisor=divisor, bw=bw, correction="none")
      pcfest$un
    },
    # Calculation of the Hs(r)-function (pair correlation function), if this is to be taken into account for the energy calculation.
    Hs = {
      hest<-spatstat.explore::Hest(spatstat.geom::ppp(x,y,window=spatstat.geom::owin(xr,yr)), correction="none")
      hest$raw
    },
    # wrong selection
    stop("unknown statistic")
    )
  )
}

compute_statistics <- function(x, y, k, xr, yr, w_statistics, bw, divisor, kernel_arg, r) {

  stat <- names(w_statistics)
  names(stat) <- stat
  lapply(stat, function(name) switch(name,
    # Calculation of the Dk(r)-function, if this is to be taken into account for the energy calculation.
    Dk = {
      nnd_ <- as.matrix(spatstat.geom::nndist(x, y, k=k))
      apply(nnd_, 2, function(z) cumsum(graphics::hist(z[z <= max(r)], breaks = c(-Inf, max(r)), plot = FALSE) $ count) / length(z))
    },

    # Calculation of the K(r)-function, if this is to be taken into account for the energy calculation.
    K = {
      kest<-spatstat.explore::Kest(spatstat.geom::ppp(x,y,window=spatstat.geom::owin(xr,yr)), rmax=max(r), correction="none")
      kest$un
    },

    # Calculation of the pcf(r)-function (spherical contact distribution), if this is to be taken into account for the energy calculation.
    pcf = {
      pcfest<-spatstat.explore::pcf.ppp(spatstat.geom::ppp(x,y,window=spatstat.geom::owin(xr,yr)), r=c(0,r), kernel=kernel_arg, divisor=divisor, bw=bw, correction="none")
      pcfest$un
    },
    # Calculation of the Hs(r)-function (pair correlation function), if this is to be taken into account for the energy calculation.
    Hs = {
      hest<-spatstat.explore::Hest(spatstat.geom::ppp(x,y,window=spatstat.geom::owin(xr,yr)), correction="none")
      hest$raw
    },
    # wrong selection
    stop("unknown statistic")
    )
  )
}
