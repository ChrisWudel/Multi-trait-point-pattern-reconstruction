#' sample_points
#'
#' @description Calculate Gest
#'
#' @param p 
#' @param size 
#'
#' @details
#'
#' @return vector
#'
#' @aliases sample_points
#' @rdname sample_points
#'
#' @keywords internal
#' 
sample_points <- function(p, size) {
  i <- sample.int(nrow(p), size, replace = TRUE)
  if (any(p$fixed_points[i])) NULL else i
}

resample <- function(x, ...) x[sample.int(length(x), ...)]