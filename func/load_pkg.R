#' load_pkg
#'
#' @description Diese Funktion überprüft, ob die angegebenen Pakete installiert sind. 
#' Falls Pakete fehlen, werden diese installiert und anschließend werden alle angegebenen 
#' Pakete in die aktuelle R-Sitzung geladen.
#'
#' @param packages Ein Vektor von Paketnamen als Zeichenfolgen, die überprüft, bei Bedarf installiert 
#' und geladen werden sollen.
#'
#' @details
#' Die Funktion identifiziert zunächst Pakete, die nicht installiert sind, installiert fehlende Pakete 
#' und lädt dann alle angegebenen Pakete in die R-Sitzung. Es wird `requireNamespace` verwendet, um zu prüfen,
#' ob das Paket verfügbar ist, und `library`, um es zu laden.
#'
#' @return NULL
#' @export
#'
#' @examples
#' # Pakete "ggplot2" und "dplyr" laden und installieren
#' load_pkg(c("ggplot2", "dplyr"))
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
