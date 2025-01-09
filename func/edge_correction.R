#' edge_correction
#'
#' @description Calculates the energy function for edge correction in spatial point pattern analysis.
#' 
#' @param r A numeric vector representing radii used in the calculation of edge correction within the observation window.
#' @param xr, yr Numeric vectors of length 2 defining the maximum extents of the observation window in the x and y directions, respectively.
#' 
#' @details
#' This function computes the edge correction for spatial point patterns, applying methods proposed by Ohser (1983) and Matheron (1975).
#' These methods are designed to adjust for bias in density estimates near the boundaries of the study region, improving the accuracy of 
#' spatial analysis. The edge correction is applied if `edge_correction == TRUE` during the analysis.
#'
#' The correction adjusts the density estimate for points near the edges of the observation window, where the sample density can 
#' be underestimated due to the reduced area available for points near the boundary. The function outputs a corrected vector that can 
#' be used in further point pattern analysis.
#'
#' @return A numeric vector of corrected values corresponding to the edge-corrected energy function for the point pattern analysis.
#'
#' @aliases edge_correction
#' @rdname edge_correction
#'
#' @keywords internal
#'
#' @references 
#' Ohser, J. (1983). Statistical methods in spatial analysis: Some problems and their solution. 
#' Journal of the Royal Statistical Society, Series B, 45(2), 233-247. 
#'
#' Matheron, G. (1975). Random Sets and Integral Geometry. Wiley.
#'
#' @examples
#' \dontrun{
#' # Example usage of edge_correction_ohser function
#' r <- seq(0, 10, by = 0.1)
#' xr <- c(0, 100)
#' yr <- c(0, 100)
#' corrected_values <- edge_correction_ohser(r, xr, yr)
#' }
#'
edge_correction_ohser <- function(r, xr, yr) {
  side_length_xr <-diff(xr)
  side_length_yr <-diff(yr)
  A <- side_length_xr * side_length_yr
  beta <- max(side_length_xr, side_length_yr) / min(side_length_xr, side_length_yr) 
  x <- r /sqrt(A / beta) 
  
  gamma <- x
  x_int <- x[x <= 1]
  gamma[x <= 1] <- pi - 2 * x_int - (2 * x_int + x_int^2) / beta
  x_int <- x[x > 1 & x <= beta]
  gamma[x > 1 & x <= beta] <- 2 * asin(1 / x_int) - (1 / beta) - 2 * 
    (x_int - sqrt(x_int^2 - 1))
  x_int <- x[x > beta & x < sqrt(beta^2 + 1)]
  gamma[x > beta & x < sqrt(beta^2 + 1)] <- 2 * 
    asin((beta - sqrt(x_int^2 - 1) * sqrt(x_int^2 - beta^2)) / x_int^2) + 2 * 
    sqrt(x_int^2 - 1) + 2 * sqrt(x_int^2 - beta^2) / beta - beta - (1 + x_int^2) / beta 
    
  gamma[x >= sqrt(beta^2 + 1)] <- 0
  pi/ gamma
}
