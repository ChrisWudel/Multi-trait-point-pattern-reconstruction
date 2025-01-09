#' select_kernel
#'
#' @description 
#' Selects a kernel function for energy calculations in point pattern reconstruction. The kernel is used to weigh distances and densities between points in the reconstruction process. 
#' Depending on the selected kernel type, different mathematical functions are used to compute these weights.
#'
#' @param kernel_arg A string specifying the kernel type to be used for energy calculations. Possible values are:
#' - `"epanechnikov"`: Epanechnikov kernel.
#' - `"rectangular"` or `"box"`: Rectangular (Box) kernel.
#' - `"gaussian"`: Gaussian kernel.
#' - `"cumulative"`: Cumulative kernel.
#'
#' @param bw A numeric value representing the bandwidth used to scale the kernel. This typically corresponds to the standard deviation in smoothing kernels like Gaussian.
#'
#' @param rmax A numeric value specifying the maximum distance at which the summary statistics are evaluated. This value affects the range of influence for the kernel function.
#'
#' @param divisor A string specifying the divisor in the kernel function. Possible values are:
#' - `"none"`: No division is applied.
#' - `"r"`: Divisor applied with respect to the distance `r`.
#' - `"d"`: Divisor applied with respect to the distance `d`.
#'
#' @details 
#' The `select_kernel` function allows you to choose from four types of kernel functions: Epanechnikov, Rectangular (Box), Gaussian, and Cumulative. These kernels are used in energy calculations during point pattern reconstruction, where the kernel defines how distances between points are weighted in the reconstruction process.
#' 
#' The function returns a list containing the selected kernel function and the adjusted `rmax` distance, which is used in further calculations for energy minimization during the point pattern reconstruction.
#'
#' The kernel functions are chosen based on the `kernel_arg` input and can be adjusted with the `bw` (bandwidth), `rmax` (maximum distance), and `divisor` (scaling factor) parameters.
#'
#' @return 
#' A list containing:
#' - `kernel`: The selected kernel function to be used for energy calculations.
#' - `rmax_bw`: The adjusted maximum distance for the kernel function.
#'
#' @aliases select_kernel
#' @rdname select_kernel
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Example of using the select_kernel function
#' kernel_info <- select_kernel(kernel_arg = "gaussian", bw = 0.5, rmax = 100, divisor = "r")
#' }
#' 
select_kernel <- function(kernel_arg, bw, rmax, divisor) {
  kernel <- switch(kernel_arg,
    epanechnikov = {
      a <- bw * sqrt(5)
      rmax_bw <- rmax + a
      switch(divisor,
        {
          rmax_bw <- sqrt(rmax^2 + a/pi)
          function(r, d) pmax.int(0, 1 - ((r^2-d^2)*pi/a)^2) * 0.75/a
        },
        none = function(r, d) pmax.int(0, 1 - ((r-d)/a)^2) * 0.75/a,
        r = function(r, d) pmax.int(0, 1 - ((r-d)/a)^2) * 0.75/(a*2*pi*r),
        d = function(r, d) pmax.int(0, 1 - ((r-d)/a)^2) * 0.75/(a*2*pi*d)
      )
    },
    rectangular =, box = {
      a <- bw * sqrt(3)
      rmax_bw <- rmax + a
      switch(divisor,
        {
          rmax_bw <- sqrt(rmax^2 + a/pi)
          function(r, d) stats::dunif((r^2-d^2)*pi,-a,+a)
        },
        none = function(r, d) stats::dunif(r,d-a,d+a),
        r = function(r, d) stats::dunif(r,d-a,d+a)/(2*pi*r),
        d = function(r, d) stats::dunif(r,d-a,d+a)/(2*pi*d)
      )
    },
    gaussian = {
      rmax_bw <- Inf
      switch(divisor,
        function(r, d) stats::dnorm((r^2-d^2)*pi,0,sd=bw),
        none = function(r, d) stats::dnorm(r,d,sd = bw),
        r = function(r, d) stats::dnorm(r,d,sd = bw)/ (2*pi*r),
        d = function(r, d) stats::dnorm(r,d,sd = bw)/ (2*pi*d)
      )
    },
    cumulative = {
      rmax_bw <- rmax
      switch(divisor,
        function(r, d) as.numeric(d <= r),
        none = function(r, d) as.numeric(d <= r),
        r = function(r, d) (d <= r) / (2*pi*r),
        d = function(r, d) (d <= r) / (2*pi*d)
      )
    }
  )
  list(kernel, rmax_bw)
}


