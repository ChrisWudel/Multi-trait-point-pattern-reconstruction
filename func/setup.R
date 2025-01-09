#' setup
#'
#' @description 
#' This function installs and loads the required R packages and additional functions for the Multi-Trait Point Pattern Reconstruction (MTPPR) workflow. 
#' It downloads the necessary R script files from GitHub, sources them into the current R session, and ensures that the specified packages and functions are available for use.
#'
#' @param packages A character vector of package names to be installed and loaded into the current R session.
#' @param func_names A character vector of function names to be loaded from predefined script files hosted on GitHub.
#'
#' @details
#' The function ensures that all the required packages and functions are installed and loaded into the R session. 
#' It first checks for the availability of the `load_pkg` and `load_func` functions by downloading the associated R scripts from GitHub.
#' After the scripts are sourced, the function uses `load_pkg` to install and load the required R packages and `load_func` to load any additional user-defined functions specified in the `func_names` parameter.
#'
#' @return NULL
#'
#' @aliases setup
#' @rdname setup
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage of the setup function
#' packages <- c("ggplot2", "dplyr")
#' func_names <- c("reconstruct_pattern_multi", "plot.rd_multi")
#' setup(packages, func_names)
#' }
#' 
setup <- function(packages, func_names) {
  # Define URLs for the R scripts
  urls <- list(
    "load_pkg" = "https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/func/load_pkg.R",
    "load_func" = "https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/func/load_func.R"
  )
  
  # Function to download and source an R script
  download_and_source_script <- function(script_name) {
    url <- urls[[script_name]]
    
    if (is.null(url)) {
      stop("URL for ", script_name, " is not defined.")
    }
    
    temp_file <- tempfile(fileext = ".R")
    tryCatch({
      # Use a temporary file to suppress download progress
      download.file(url, temp_file, quiet = TRUE)
      
      if (file.exists(temp_file)) {
        # Suppress messages and warnings from source
        suppressMessages(suppressWarnings({
          source(temp_file, local = FALSE)
        }))
        unlink(temp_file)
      } else {
        stop("Failed to download script from ", url)
      }
    }, error = function(e) {
      stop("Error downloading or sourcing ", script_name, ": ", e$message)
    })
  }
  
  # Ensure that the `load_pkg` function is available
  download_and_source_script("load_pkg")
  if (!exists("load_pkg", mode = "function")) {
    stop("The function `load_pkg` is not available.")
  }
  
  # Call the function to install and load packages
  load_pkg(packages)
  
  # Ensure that the `load_func` function is available
  download_and_source_script("load_func")
  if (!exists("load_func", mode = "function")) {
    stop("The function `load_func` is not available.")
  }
  
  # Call the function to load additional functions
  load_func(func_names)
}