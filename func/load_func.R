#' load_func
#'
#' @description Loads predefined functions for Multi-Trait Point Pattern Reconstruction (MTPPR) from external R scripts.
#'
#' @param func_names A character vector of function names to load. Each element should be a string corresponding 
#' to the name of a function defined in the predefined R script files. These names must match the ones 
#' specified in the predefined URLs for R scripts.
#'
#' @details
#' This function loads R functions from external script files, which are hosted on platforms like GitHub. 
#' The URLs of these R script files are predefined within the function. The function fetches the R script 
#' from the given URLs and evaluates the code, making the functions available in the current R environment.
#' After execution, the requested functions can be accessed and used by their names, provided they are 
#' successfully loaded.
#'
#' @return A vector of character strings containing the names of the functions that were successfully 
#' loaded into the environment.
#'
#' @aliases load_func
#' @rdname load_func
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Example usage of load_func function
#' # Load the 'reconstruct_pattern_multi' and 'compute_statistics' functions
#' load_func(c("reconstruct_pattern_multi", "compute_statistics"))
#' }
#'
load_func <- function(func_names) {
  # Ensure that 'func_names' is a character vector
  if (!is.character(func_names)) {
    stop("The 'func_names' argument must be a character vector.")
  }

  # Define the URLs where the R script files are hosted
  urls <- c(
    "reconstruct_pattern_multi" = "https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/func/reconstruct_pattern_multi.R",
    "compute_statistics" = "https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/func/compute_statistics.R",
    "dummy_transf" = "https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/func/dummy_transf.R",
    "energy_fun" = "https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/func/energy_fun.R",
    "calc_moments" = "https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/func/calc_moments.R",
    "select_kernel" = "https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/func/select_kernel.R",
    "plot.rd_multi" = "https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/func/plot.rd_multi.R",
    "sample_points" = "https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/func/sample_points.R",
    "select_data" = "https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/func/select_data.R",
    "vis_patterns" = "https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/func/vis_patterns.R",
    "plot_statistics" = "https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/func/plot_statistics.R",
    "edge_correction" = "https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/func/edge_correction.R"
  )
  
  # Check if there are any requested functions that are not defined in the URLs list
  missing_functions <- setdiff(func_names, names(urls))
  if (length(missing_functions) > 0) {
    stop("The following functions are not defined: ", paste(missing_functions, collapse = ", "))
  }
  
  # Loop through each requested function
  for (function_name in func_names) {
    # Get the URL for the current function
    url <- urls[[function_name]]
    
    # Create a temporary file to download the script
    temp_file <- tempfile(fileext = ".R")
    
    # Attempt to download the script and source it
    tryCatch({
      # Download the R script from the specified URL
      download.file(url, temp_file)
      # Source the R script to load the function definitions
      source(temp_file, local = FALSE)
    }, error = function(e) {
      # If an error occurs, issue a warning message
      warning("Failed to load function '", function_name, "': ", e$message)
    })
    
    # Delete the temporary file
    unlink(temp_file)
  }
  
  # Return the names of the functions that were successfully loaded (invisible)
  invisible(func_names)
}