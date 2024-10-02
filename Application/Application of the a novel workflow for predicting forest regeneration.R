################################################################################
# Application of the point pattern reconstruction with two marks, for the      #
#                          three available data sets.                          #
################################################################################

# Source and setup environment
source("https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/func/setup.R")

setup(
  packages = c("spatstat", "ggplot2"),  # Packages to install and load
  func_names = c("reconstruct_pattern_multi", "compute_statistics", # Functions to load
                 "dummy_transf", "energy_fun", "calc_moments", "select_kernel", 
                 "plot.rd_multi", "sample_points", "edge_correction", "vis_patterns")
)

##################################################################################
# Query whether and which visualisations are to be carried out.
visualisation_of_point_patterns <- TRUE  # Set to TRUE or FALSE as needed
##################################################################################
# Selection of the dataset, which is then imported via GitHub.
url <- "https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/Records/Real_datasets/Terrestrial_laser_scan_data_340_a31.csv"
data <- read.csv(url, sep = ",", stringsAsFactors = TRUE)
data$dbh <- as.numeric(data$dbh)

data <- data.frame(data[2],data[3],data[4],data[5])
colnames(data) <- c("x", "y", "dbh", "species")
##################################################################################
# Execution of the point pattern reconstruction function.
W <- owin(c(0, 100), c(0, 100))  # Spatial window
core_window <- owin(c(35, 65), c(35, 65))  # Core window for the reconstruction

marked_pattern <- as.ppp(data.frame(data), W = W)  
marked_pattern$marks$dbh <- marked_pattern$marks$dbh * 0.001  # Convert dbh to meters

reconstruction <- reconstruct_pattern_multi(                                    ## The description of the parameters can be found in the reconstruct_pattern_multi.R function in the func folder.
  marked_pattern,
  core_window = core_window,
  fixed_points = NULL,
  edge_correction = TRUE,     
  n_repetitions = 1,     
  max_steps = 200000,     
  no_change = 5,     
  rcount = 250,     
  rmax = 25,      
  issue = 1000,       
  divisor = "r",    
  kernel_arg = "epanechnikov",
  timing = TRUE,    
  energy_evaluation = TRUE,
  show_graphic = FALSE,  
  Lp = 1,      
  bw = 0.5,
  sd = "step",
  steps_tol = 10000,   
  tol = 1e-4,   
  w_markcorr = c(m_m = 1, one_one = 1500, all = 1, m_all = 1, all_all = 1, m_m0 = 1, one_one0 = 1500, all0 = 1, m_all0 = 1, all_all0 = 1),
  prob_of_actions = c(move_coordinate = 0.3, switch_coords = 0.1, exchange_mark_one = 0.1, exchange_mark_two = 0.1, pick_mark_one = 0.1, pick_mark_two = 0.1, delete_point = 0.1, add_point = 0.1), 
  k = 1,       
  w_statistics = c(),              
  is.fixed = function(p) 35 <= p$x & p$x <= 65 & 35 <= p$y & p$y <= 65 | p$mark[,"dbh"] > 0.1,
  verbose = TRUE
)

##################################################################################
# Loads and executes the function for visualizing the point patterns if TRUE.
if (visualisation_of_point_patterns) { 
  vis_pp(reconstruction) 
}
