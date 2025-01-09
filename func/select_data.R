#' data_import
#'
#' @description Imports various datasets based on the input parameter and processes them.
#' 
#' @param x A string value specifying which dataset to load and process. Possible values include:
#' - `"VERMOS_project"`: Dataset from the VERMOS project.
#' - `"Northwest_German_Forest_Research_Institute"`: Dataset from the Northwest German Forest Research Institute.
#' - `"Marteloscope_data_from_the_by_the_Chair_of_Forest_Growth_and_Woody_Biomass_Production"`: Marteloscope dataset.
#' - `"random"`: Simulated random point pattern dataset.
#' - `"regular"`: Simulated regular point pattern dataset.
#' - `"cluster_size5"`: Simulated clustered point pattern dataset with cluster size 5.
#' - `"cluster_size5_and_random"`: Simulated clustered point pattern dataset with cluster size 5 and random points.
#' 
#' @details
#' This function loads different datasets based on the value of the `x` parameter, processes the dataset, 
#' and returns a list containing the dataset and its corresponding spatial window (`W`).
#' The datasets include both real-world and simulated point patterns, with attributes such as position (`x`, `y`), 
#' diameter at breast height (`dbh [mm]`), and tree species.
#' The spatial window object (`W`) defines the spatial extent of the data for further analysis.
#'
#' @return A list containing:
#' - `data`: A data frame with the columns `x`, `y`, `dbh [mm]`, and `Tree species`.
#' - `W`: A spatial window object representing the spatial extent of the dataset.
#'
#' @aliases data_import
#' @rdname data_import
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example of importing the VERMOS project dataset
#' data_info <- data_import("VERMOS_project")
#' data <- data_info[[1]]
#' spatial_window <- data_info[[2]]
#' }
#' 
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

