#' energy_fun
#'
#' @description Energy function
#'
#' @param f Result of the calc_moments_full function which represents
#' product-moment contribution of a point at coordinates x, y with marks,
#' for the whole new ponit pattern.
#' @param f0  Column sums of the weights of the brand correlation functions of
#' the new point pattern.
#' @param statistics Results of the compute_statistics function for the
#' new point pattern (calculation of optional spatial statistics).
#' @param fn Determination of the weightings of the mark correlation functions.
#' @param wedge 
#' @param wedge_ 
#' @param Lp Distance measure for the calculation of the energy function
#' (Lp distance, 1 <= p <Inf).
#' @param w_statistics Vector of named weights for optional spatial statistics
#' from the \code{spatstat} package to be included in the energy calculation.This may
#' include Dk, K, Hs, pcf.
#'
#' @details
#' Defining the Energy_fun function to calculate the "energy" of the pattern
#' (where a lower energy indicates a better match).
#'
#' @return vector
#'
#' @aliases energy_fun
#' @rdname energy_fun
#'
#' @keywords internal
#'
Energy_fun <- function(f, f0, n, statistics, f_, f0_, n_, statistics_, fn, wedge, wedge_, Lp, w_statistics) {
     result <- c(
      f = sum(fn$w * rowMeans(abs(
        f * wedge / n -
        f_ * wedge_ / n_
      )^Lp)),
      f0 = sum(fn$w0 * abs(
        f0 / n - 
        f0_ / n_
      )^Lp),
      if (length(w_statistics))
        sapply(seq_along(w_statistics), function(i) w_statistics[i] *
          mean(abs(statistics[[i]] - statistics_[[i]])^Lp, na.rm = TRUE),
          USE.NAMES=FALSE
        )
    )
    c(energy = sum(result), result)
}
