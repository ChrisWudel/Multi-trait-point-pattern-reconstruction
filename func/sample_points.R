#' sample_points
#'
#' @description 
#' Samples a specified number of points from a dataframe based on the values in the `fixed_points` column. The function ensures that the sampled points correspond to 
#' those that are marked as fixed in the dataset. If no fixed points are available for sampling, the function will return `NULL`.
#'
#' @param p A dataframe that must contain a column named `fixed_points`. This column indicates whether each point is fixed or not.
#' 
#' @param size The number of points to sample from the dataframe. This is the number of rows that will be randomly selected.
#'
#' @details 
#' The function first samples `size` rows from the input dataframe `p` using random sampling with replacement. It then checks whether any of the sampled points are marked as fixed in the `fixed_points` column. 
#' If all sampled points are fixed, it returns the sampled points. If none of the sampled points are fixed, the function returns `NULL`.
#'
#' @return 
#' A vector containing the indices of the sampled points if any of them are marked as fixed. If no fixed points are sampled, the function returns `NULL`.
#'
#' @aliases sample_points
#' @rdname sample_points
#'
#' @keywords internal
#' @export
#' 
sample_points <- function(p, size) {
  i <- sample.int(nrow(p), size, replace = TRUE)
  if (any(p$fixed_points[i])) NULL else i
}

#' resample
#'
#' @description 
#' Resamples a vector by randomly selecting a specified number of elements, with replacement.
#'
#' @param x A vector from which to sample.
#' @param ... Additional arguments passed to the sampling function, such as the number of elements to sample.
#'
#' @details 
#' This function randomly samples elements from the vector `x`, with the option to specify how many elements to sample through the `...` argument.
#'
#' @return 
#' A resampled vector with the selected elements from the original vector.
#'
#' @keywords internal
#' @export
#' 
resample <- function(x, ...) x[sample.int(length(x), ...)]