#' edge_correction
#'
#' @description Energy function
#'
#' @param r 
#' @param xr  
#' @param yr 
#'
#' @details
#' To calculate edge correction for spatial point pattern analysis, we consider 
#' methods proposed by Ohser (1983) and Matheron (1975). These methods adjust for 
#' bias in density estimates near the boundaries of a study region 
#' if edge_correction == TRUE
#'
#' @return vector
#'
#' @aliases edge_correction
#' @rdname edge_correction
#'
#' @keywords internal
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
