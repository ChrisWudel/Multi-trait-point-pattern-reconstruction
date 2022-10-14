################################################################################
# Application of the point pattern reconstruction with two marks, for the      #
#                          three available data sets.                          #
################################################################################
#install.packages("spatstat")                                                    ## Packages which are necessary for the execution of the point pattern reconstruction, please install if not available.  
library(spatstat)  

#install.packages("ggplot2")                                                     ## Packages required to run the entire script (point pattern reconstruction + visualisation). Please install them if they are not present when you want to run the script.
#install.packages("patchwork")
#install.packages("plotly")
#install.packages("reshape")
library(ggplot2)
library(patchwork)
library(plotly)
library(reshape)
                                                                                ## Loading function from github.
source("https://raw.githubusercontent.com/ChrisWudel/Point-pattern-reconstruction/main/Point%20pattern%20reconstruction%20with%20two%20marks%20.R")

################################################################################## Query whether and which visualisations are to be carried out.
visualisation_of_point_patterns     <- TRUE # or FALSE # the library(ggplot2) and the library(patchwork) must be installed.
visualisation_of_summary_statistics <- TRUE # or FALSE # the library(ggplot2) and the library(reshape) must be installed.
energy_course                       <- TRUE # or FALSE # the library(plotly) must be installed.

################################################################################## Selection of the date set, which is then imported via gihub.
source("https://raw.githubusercontent.com/ChrisWudel/Point-pattern-reconstruction/main/Records/Function%20to%20select%20one%20of%20the%20three%20available%20data%20sets.R")
x <- "VERMOS_project" # The following three data sets can be imported:                
                                   #    "VERMOS_project" 
                                   #    "Northwest_German_Forest_Research_Institute" 
                                   #    "Marteloscope_data_from_the_by_the_Chair_of_Forest_Growth_and_Woody_Biomass_Production"
                                   # to do this, declare x with the corresponding name in "". 
data <- data_import(x)
W <- data[[2]] 
data <- data [[1]]

################################################################################## Execution of the point pattern reconstruction function.
marked_pattern <- as.ppp(data.frame(data), W = W)  
reconstruction <- Pattern_reconstruction_with_two_marks(
                    marked_pattern, 
                    n_repetitions     = 1,
                    max_runs          = 10000, #100000                          ## Use for the datasets Northwest_German_Forest_Research_Institute and 
                    no_changes        = 5,                                      ## Marteloscope_data_from_the_by_the_Chair_of_Forest_Growth_and_Woody_Biomass_Production 
                    rcount            = 250,                                    ## for max_runs 10000 and rmax 25.
                    rmax              = 5,     # 25                             
                    issue             = 1000,
                    use.g_func        = TRUE,
                    divisor           = "d", 
                    timing            = TRUE, 
                    energy_evaluation = TRUE,
                    show_graphic      = TRUE,
                    bw                = 0.5)

################################################################################## Loads and executes the function for visualising the point patterns under consideration if TURE. 
if(visualisation_of_point_patterns  == TRUE){ 
  source("https://raw.githubusercontent.com/ChrisWudel/Point-pattern-reconstruction/main/Visualisation/Function%20for%20the%20visualisation%20of%20the%20considered%20point%20patterns.R")
  vis_pp(reconstruction) 
}

################################################################################## Loads and executes the summary statistics visualisation function if TURE.
if(visualisation_of_summary_statistics == TRUE){
  source("https://raw.githubusercontent.com/ChrisWudel/Point-pattern-reconstruction/main/Visualisation/Function%20for%20the%20visualisation%20of%20summary%20statistics%20(markcorr%2C%20pcf%20%2C%20Gest).R")
  plot_sum_stat(reconstruction)
}

################################################################################## Loads and executes the energy course visualisation function if TURE.
if(energy_course == TRUE){
   source("https://raw.githubusercontent.com/ChrisWudel/Point-pattern-reconstruction/main/Visualisation/Function%20for%20the%20visualisation%20of%20the%20energy%20course.R")
   vis_ener_cour(reconstruction)
}
