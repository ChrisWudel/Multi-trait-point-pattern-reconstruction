#' dummy_transf
#'
#' @description Transforms variables to dummy variables and vice versa. 
#' This function provides the ability to convert categorical variables into a 
#' series of binary indicators (dummy variables), and also supports converting 
#' dummy variables back to their original categorical form.
#'
#' @param f A factor or numeric vector representing the categorical data, 
#' typically the result of a previous calculation like the `calc_moments_full` function. 
#' It represents the product-moment contribution of a point at coordinates \(x\), \(y\) 
#' with marks for the entire point pattern.
#' 
#' @param x A matrix where each column corresponds to a set of dummy variables, 
#' representing a categorical variable. Typically the result of the `to.dummy` function.
#' @param levels An optional character vector that defines the levels of the categorical variable 
#' in the original form when converting from dummy variables back to the factor format. Defaults to the column names of `x`.
#'
#' @details
#' The function `to.dummy` transforms a factor (or a categorical variable) into a matrix of dummy variables. 
#' Each level of the factor is represented as a binary column, where `1` indicates the presence of that level, 
#' and `0` indicates its absence. 
#'
#' The function `from.dummy` performs the reverse operation, converting a matrix of dummy variables back into 
#' a factor, using the provided levels.
#'
#' @return 
#' - `to.dummy`: A matrix where each column corresponds to a binary dummy variable representing each level of the factor `f`. 
#' - `from.dummy`: A factor representing the original categorical variable, reconstructed from the dummy variables.
#'
#' @aliases dummy_transf
#' @rdname dummy_transf
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Example of transforming a factor to dummy variables
#' factor_data <- factor(c("A", "B", "A", "C", "B"))
#' dummy_data <- to.dummy(factor_data)
#' 
#' # Example of converting dummy variables back to the original factor
#' original_factor <- from.dummy(dummy_data)
#' }
#' 
to.dummy <- function(f) {
  x <- matrix(0, length(f), nlevels(f), dimnames=list(names(f), levels(f)))
  x[cbind(seq_along(f), as.integer(f))] <- 1
  x
}

from.dummy <- function(x, levels=colnames(x)) {
  f <- as.integer(x %*% seq_along(levels))
  levels(f) <- levels
  class(f) <- "factor"
  f
}
