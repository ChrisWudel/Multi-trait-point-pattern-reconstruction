#' data_import
#'
#' @description Imports various datasets.
#' 
#' @param x Input value that determines which dataset should be loaded and processed.
#' 
#' @details
#' This function is used to import various datasets based on an input
#' value, process them, and return them as a list containing the data and a spatial
#' window object (W).
#'
#' @return void
#'
#' @aliases data_import
#' @rdname data_import
#'
#' @export
data_import <- function(x) {
  
switch(x,
       VERMOS_project = { 
         url <-"https://raw.githubusercontent.com/ChrisWudel/point-pattern-reconstruction/main/Records/Real_datasets/Individual_tree_data_5138_Wudel%20et%20al.%2C%202022.csv"
         data <- read.csv(url,sep = ",", stringsAsFactors = TRUE)
         data <- data.frame(data$x, data$y, data$dbh..mm., data$Tree.species)
         colnames(data) <- c("x", "y", "dbh [mm]", "Tree species")
         W <- owin( c(0, 10 ), c(0, 10))
       },
       Northwest_German_Forest_Research_Institute = { 
         url <-"https://raw.githubusercontent.com/ChrisWudel/point-pattern-reconstruction/main/Records/Real_datasets/stammv_3002j_Spellmann%2C%202022.csv"
         data <- read.csv(url,sep = ",", stringsAsFactors = TRUE)
         data <-data.frame(data$x, data$y, as.numeric(data$d), data$Tree.species)
         colnames(data)<-c("x", "y", "dbh [mm]", "Tree species")
         W <- owin(c(0, 160), c(0, 160))
       },
       Marteloscope_data_from_the_by_the_Chair_of_Forest_Growth_and_Woody_Biomass_Production = { 
         url <-"https://raw.githubusercontent.com/ChrisWudel/point-pattern-reconstruction/main/Records/Real_datasets/Marteloscope_710_b1_Fichtner%20and%20van%20der%20Maaten-Theunissen%2C%202022.csv"
         data <- read.csv(url,sep = ",", stringsAsFactors = TRUE)
         data <- data.frame(data$x..m., data$y..m., data$dbh..cm., data$Tree.species)
         data$data.dbh..cm. <- data$data.dbh..cm. / 0.1
         colnames(data)<-c("x", "y", "dbh [mm]", "Tree species")
         W <- owin(c(0, 70), c(0, 140))
       },
       random = { 
         url <-"https://raw.githubusercontent.com/ChrisWudel/point-pattern-reconstruction/main/Records/Simulated_patterns/random.csv"
         data <- read.csv(url,sep = ",", stringsAsFactors = TRUE)
         data <-data.frame(data$x, data$y, as.numeric(data$d), data$Tree.species)
         colnames(data)<-c("x", "y", "dbh [mm]", "Tree species")
         W <- owin(c(0, 500), c(0, 1000))
       },
       regular = { 
         url <-"https://raw.githubusercontent.com/ChrisWudel/point-pattern-reconstruction/main/Records/Simulated_patterns/regular.csv"
         data <- read.csv(url,sep = ",", stringsAsFactors = TRUE)
         data <-data.frame(data$x, data$y, as.numeric(data$d), data$Tree.species)
         colnames(data)<-c("x", "y", "dbh [mm]", "Tree species")
         W <- owin(c(0, 500), c(0, 1000))
       },
       cluster_size5 = { 
         url <-"https://raw.githubusercontent.com/ChrisWudel/point-pattern-reconstruction/main/Records/Simulated_patterns/cluster_size5.csv"
         data <- read.csv(url,sep = ",", stringsAsFactors = TRUE)
         data <-data.frame(data$x, data$y, as.numeric(data$d), data$Tree.species)
         colnames(data)<-c("x", "y", "dbh [mm]", "Tree species")
         W <- owin(c(0, 500), c(0, 1000))
       },
        cluster_size5_and_random = { 
         url <-"https://raw.githubusercontent.com/ChrisWudel/point-pattern-reconstruction/main/Records/Simulated_patterns/cluster_size5_and_random.csv"
         data <- read.csv(url,sep = ",", stringsAsFactors = TRUE)
         data <-data.frame(data$x, data$y, as.numeric(data$d), data$Tree.species)
         colnames(data)<-c("x", "y", "dbh [mm]", "Tree species")
         W <- owin(c(0, 500), c(0, 1000))
})

data <- list(data, W)
return(data)
}

