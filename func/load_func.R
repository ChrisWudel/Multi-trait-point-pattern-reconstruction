#' load_func
#'
#' @description Loads the desired predefined functions for MTPPR (Multi-Trait Point Pattern Reconstruction).
#'
#' @param function_names A vector of character strings. Each string is the name of a 
#' function you want to load and use. These function names should match those 
#' defined in the R script files specified by the URLs.
#'
#' @details
#' The function loads R functions from R script files hosted on GitHub or other web sources. 
#' The URLs of these R script files are predefined within the function. The function retrieves 
#' and evaluates the code from these scripts to make the functions available in the R environment. 
#' Once the code is executed, you can access and use the functions specified by their names.
#'
#' @return A vector of character strings containing the names of the successfully loaded functions.
#'
#' @aliases load_func
#' @rdname load_func
#'
#' @keywords internal
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