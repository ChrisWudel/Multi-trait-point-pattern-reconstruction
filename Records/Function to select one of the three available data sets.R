################################################################################
#           Function to select one of the three available data sets            #
################################################################################
data_import <- function(x) {
  
switch(x,
       VERMOS_project = { 
         url <-"https://raw.githubusercontent.com/ChrisWudel/Point-pattern-reconstruction/main/Records/Individual_tree_data_5138_Wudel et al., 2022_doi.org_10.5281_zenodo.7157076.csv"
         data <- read.csv(url,sep = ",", stringsAsFactors = TRUE)
         data <- data.frame(data$x, data$y, data$dbh..mm., data$Tree.species)
         colnames(data) <- c("x", "y", "dbh [mm]", "Tree species")
         W <- owin( c(0, 10 ), c(0, 10))
       },
       Northwest_German_Forest_Research_Institute = { 
         url <-"https://raw.githubusercontent.com/ChrisWudel/Point-pattern-reconstruction/main/Records/stammv_3002j_Spellmann, 2022_doi.org_10.5281_zenodo.7124817.csv"
         data <- read.csv(url,sep = ",", stringsAsFactors = TRUE)
         data <-data.frame(data$x, data$y, as.numeric(data$d), data$Tree.species)
         colnames(data)<-c("x", "y", "dbh [mm]", "Tree species")
         W <- owin(c(0, 160), c(0, 160))
       },
       Marteloscope_data_from_the_by_the_Chair_of_Forest_Growth_and_Woody_Biomass_Production = { 
         url <-"https://raw.githubusercontent.com/ChrisWudel/Point-pattern-reconstruction/main/Records/Marteloscope_710_b1_Fichtner and van der Maaten-Theunissen, 2022_doi.org_10.5281_zenodo.7147868.csv"
         data <- read.csv(url,sep = ",", stringsAsFactors = TRUE)
         data <- data.frame(data$x..m., data$y..m., data$dbh..cm., data$Tree.species)
         data$data.dbh..cm. <- data$data.dbh..cm. / 0.1
         colnames(data)<-c("x", "y", "dbh [mm]", "Tree species")
         W <- owin(c(0, 70), c(0, 140))
})

data <- list(data, W)
return(data) 
  
}