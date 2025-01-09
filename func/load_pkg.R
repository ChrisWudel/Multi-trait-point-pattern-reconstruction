#' load_pkg
#'
#' @description Checks if the specified packages are installed. If any packages are missing, 
#' they are installed, and then all specified packages are loaded into the current R session.
#'
#' @param packages A character vector of package names. The function checks whether these packages 
#' are installed, installs any that are missing, and then loads all specified packages into the current R session.
#'
#' @details
#' The function first checks if the specified packages are already installed in the current R environment 
#' using `requireNamespace`. If any packages are missing, they will be automatically installed using `install.packages()`. 
#' After installation, the function loads all the specified packages into the current R session using `library()`. 
#' This ensures that the required packages are available for use in the session.
#'
#' @return NULL
#' 
#' @examples
#' # Install and load the "ggplot2" and "dplyr" packages
#' load_pkg(c("ggplot2", "dplyr"))
#' 
#' @export
#' 
load_pkg <- function(packages) {
  # Sicherstellen, dass 'packages' ein Zeichenvektor ist
  if (!is.character(packages)) {
    stop("Das Argument 'packages' muss ein Zeichenvektor sein.")
  }
  
  # Überprüfen, ob Pakete fehlen
  nicht_installiert <- packages[!(packages %in% installed.packages()[, "Package"])]
  
  if (length(nicht_installiert) > 0) {
    message("Fehlende Pakete werden installiert: ", paste(nicht_installiert, collapse = ", "))
    
    tryCatch({
      install.packages(nicht_installiert, dependencies = TRUE)
    }, error = function(e) {
      stop("Fehler beim Installieren der Pakete: ", e$message)
    })
    
    # Erneute Überprüfung, ob alle Pakete installiert wurden
    nicht_installiert <- nicht_installiert[!(nicht_installiert %in% installed.packages()[, "Package"])]
    if (length(nicht_installiert) > 0) {
      stop("Die folgenden Pakete konnten nicht installiert werden: ", paste(nicht_installiert, collapse = ", "))
    }
  } else {
    message("Alle Pakete sind bereits installiert.")
  }
  
  # Alle angegebenen Pakete laden
  invisible(lapply(packages, function(pkg) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      library(pkg, character.only = TRUE)
      message("Paket erfolgreich geladen: ", pkg)
    } else {
      message("Paket konnte nicht geladen werden: ", pkg)
    }
  }))
}
