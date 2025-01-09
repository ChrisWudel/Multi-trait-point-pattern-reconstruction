#' energy_fun
#'
#' @description Computes the energy function for spatial point pattern analysis.
#'
#' @param f A numeric vector representing the result of the `calc_moments_full` function, 
#' which contains the product-moment contributions for each point in the new point pattern, 
#' calculated for the entire point pattern.
#' @param f0 A numeric vector containing the column sums of the weights of the mark correlation 
#' functions of the new point pattern.
#' @param n The number of observations in the new point pattern. This is used for normalizing 
#' `f` and `f0` during the energy calculation.
#' @param statistics A list of numeric vectors or matrices containing the results from the 
#' `compute_statistics` function for the new point pattern. This includes optional spatial 
#' statistics like `Dk`, `K`, `Hs`, or `pcf`.
#' @param f_, f0_, n_, statistics_ Same as `f`, `f0`, `n`, and `statistics`, but for the reference 
#' point pattern (used to compare against the new point pattern).
#' @param fn A list representing the determination of the weightings of the mark correlation 
#' functions for the new point pattern.
#' @param wedge A numeric vector or matrix representing the weighting factors used to adjust the 
#' significance of different values during the energy calculation. Larger values increase the 
#' contribution of corresponding data points.
#' @param Lp A numeric value representing the distance measure used in the energy function 
#' calculation (Lp distance, where \(1 \leq p < \infty\)).
#' @param w_statistics A named vector of weights for optional spatial statistics (e.g., `Dk`, 
#' `K`, `Hs`, or `pcf`) from the `spatstat` package. These statistics are used in the energy calculation.
#'
#' @details
#' The energy function quantifies the "energy" of a spatial point pattern, which is a measure of 
#' the discrepancy between the observed point pattern (new pattern) and a reference pattern. The 
#' goal is to minimize the energy, which implies a better fit between the observed and reference patterns.
#' The energy is calculated by comparing the weighted moments, spatial statistics, and correlations 
#' between the new and reference point patterns, with additional weightings applied through the `wedge` 
#' and `w_statistics` parameters.
#'
#' The function computes the energy by considering:
#' - The difference in the weighted product-moments between the new and reference patterns.
#' - The difference in the mark correlation functions (`f0`).
#' - The difference in spatial statistics (such as `Dk`, `K`, `Hs`, or `pcf`).
#' A lower energy value indicates a better match between the new point pattern and the reference pattern.
#'
#' @return A numeric vector representing the total energy value of the point pattern, 
#' along with the individual components (`f`, `f0`, and spatial statistics differences).
#'
#' @aliases energy_fun
#' @rdname energy_fun
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Example usage of energy_fun function
#' energy_values <- energy_fun(f, f0, n, statistics, f_, f0_, n_, statistics_, fn, wedge, wedge_, Lp, w_statistics)
#' }
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
