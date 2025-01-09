#' calc_moments
#'
#' @description This function calculates the product-moment function for a given point pattern, 
#' based on specified parameters. It computes the weighted correlation of marks between points 
#' in the point pattern using a smoothing kernel.
#'
#' @param fn A function that defines the weightings of the mark correlation functions. 
#' It should specify the indices of points (i, j) for the moment calculation.
#' @param p A point pattern object that contains the coordinates and marks (attributes) of the points.
#' @param x A numeric vector containing the x-coordinates of the points from the reference point pattern.
#' @param y A numeric vector containing the y-coordinates of the points from the reference point pattern.
#' @param mark A vector of marks (attributes) for the points in the point pattern.
#' @param kernel The kernel function resulting from the kernel calculation (typically from `calc_kernels`).
#' @param rmax_bw The maximum distance at which summary statistics are evaluated, which also defines the bandwidth 
#' for kernel scaling. This parameter determines the standard deviation of the smoothing kernel.
#' @param r A numeric vector representing a sequence of radii used in the kernel estimation, ranging from `rmin` to `rmax`.
#' @param exclude A vector of indices that specifies which points should be excluded from the calculation.
#'
#' @details
#' This function calculates the product-moment function for each point in the point pattern. The product-moment 
#' function assesses the contribution of a point at coordinates \( x \), \( y \), with a corresponding mark, using kernel 
#' smoothing to evaluate the distance-based correlation of the marks. The distance between points is evaluated and the 
#' kernel function is applied for smoothing.
#'
#' @return A numeric matrix where each element corresponds to a calculated moment based on the mark correlation 
#' for the selected points.
#'
#' @aliases calc_moments
#' @rdname calc_moments
#'
#' @keywords internal
#' 
#' @examples
#' \dontrun{
#' # Example usage of calc_moments
#' moments <- calc_moments(fn = some_function, p = some_point_pattern, x = x_coords, y = y_coords, 
#'                         mark = marks, kernel = some_kernel, rmax_bw = 50, r = seq(0, 100, 10))
#' }
#' 
calc_moments <- function(fn,
                         p,
                         exclude = NULL,
                         x,
                         y,
                         mark,
                         kernel,
                         rmax_bw,
                         r) {

  d2 <- (p$x-x)^2 + (p$y-y)^2
  use <-  d2 <= rmax_bw^2
  use[exclude] <- FALSE
  z <- crossprod(p$mark[use, , drop = FALSE],
                 outer(sqrt(d2[use]), r, function(d, r) kernel(r, d)))
  z[fn$i, , drop = FALSE] * mark[fn$j] + z[fn$j, , drop = FALSE] * mark[fn$i]
}

#' calc_moments_full
#'
#' @description This function computes the full matrix of moments for a point pattern, 
#' summing the product-moment contributions for all pairs of points.
#'
#' @param fn A function that defines the weightings of the mark correlation functions, 
#' specifying indices of points (i, j) for moment calculation.
#' @param p A point pattern object containing the coordinates and marks (attributes) of the points.
#' @param kernel The kernel function resulting from the kernel calculation (typically from `calc_kernels`).
#' @param rmax_bw The maximum distance at which summary statistics are evaluated, 
#' also defining the bandwidth for kernel scaling (standard deviation of the smoothing kernel).
#' @param r A numeric vector representing a sequence of radii for kernel estimation, ranging from `rmin` to `rmax`.
#'
#' @details
#' This function calculates the full matrix of moments for the entire point pattern by iterating through all pairs of points 
#' and summing their individual product-moment contributions. The result is a matrix that reflects the weighted correlation 
#' of marks for all point pairs based on their spatial proximity and the kernel function.
#'
#' @return A matrix containing the calculated moments for each pair of points in the pattern, 
#' with row names representing the pairs of point indices (i, j).
#'
#' @aliases calc_moments_full
#' @rdname calc_moments_full
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Example usage of calc_moments_full
#' full_moments <- calc_moments_full(fn = some_function, p = some_point_pattern, 
#'                                    kernel = some_kernel, rmax_bw = 50, r = seq(0, 100, 10))
#' }
#' 
calc_moments_full <- function(fn,
                              p,
                              kernel,
                              rmax_bw,
                              r) {

  f <- 0
  for (i in seq_len(nrow(p))) {
    f <- f + calc_moments(fn, p, i:nrow(p), p$x[i], p$y[i], p$mark[i, ],
                          kernel, rmax_bw, r)
  }
  rownames(f) <- paste(names(fn$i), names(fn$j), sep = ":")
  f
}
