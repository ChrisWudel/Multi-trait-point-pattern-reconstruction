################################################################################
# Application of the point pattern reconstruction with two marks, for the      #
#                          three available data sets.                          #
################################################################################

# Source and setup environment
source("https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/func/setup.R")

setup(
  packages = c("spatstat", "ggplot2", "patchwork", "plotly", "reshape"),  # Packages to install and load
  func_names = c("reconstruct_pattern_multi", "compute_statistics",       # Functions to load
                 "dummy_transf", "energy_fun", "calc_moments", "select_kernel",
                 "plot.rd_multi", "sample_points", "select_data","vis_patterns",
                 "plot_statistics")
)

##################################################################################
# Query whether and which visualisations are to be carried out.
visualisation_of_point_patterns     <- TRUE  # or FALSE, depending on your need
visualisation_of_summary_statistics <- TRUE  # or FALSE, depending on your need

##################################################################################
# Selection of the data set, which is then imported via GitHub.
x <- "random"  # Select dataset: "VERMOS_project", "Northwest_German_Forest_Research_Institute", 
               # "Marteloscope_data_from_the_by_the_Chair_of_Forest_Growth_and_Woody_Biomass_Production",
               # "random", "regular", "cluster_size5", "cluster_size5_and_random"
data <- data_import(x)

# Extract spatial window and data
W <- data[[2]]
data <- data[[1]]

##################################################################################
# Execution of the point pattern reconstruction function.
marked_pattern <- as.ppp(data.frame(data), W = W)

# Adjust marks (diameter in meters)
marked_pattern$marks[1] <- marked_pattern$marks[1] * 0.001

# Perform reconstruction
reconstruction <- reconstruct_pattern_multi(
  marked_pattern, 
  n_repetitions     = 1,
  max_steps         = 10000,
  no_change         = 5,
  rcount            = 250,
  rmax              = 25,
  issue             = 1000,
  divisor           = "r",
  kernel_arg        = "epanechnikov",
  timing            = TRUE,
  energy_evaluation = TRUE,
  show_graphic      = FALSE,
  Lp                = 1,
  bw                = 0.5,
  sd                = "step",
  steps_tol         = 1000,
  tol               = 1e-4,
  w_markcorr        = c(m_m=1,one_one=0,all=1,m_all=1,all_all=1,m_m0=1,one_one0=0,all0=1,m_all0=1,all_all0=1),
  prob_of_actions   = c(move_coordinate = 0.4,
                         switch_coords = 0.1,
                         exchange_mark_one = 0.1,
                         exchange_mark_two = 0.1,
                         pick_mark_one = 0.2,
                         pick_mark_two = 0.1,
                         delete_point = 0.0, 
                         add_point = 0.0),
  k                 = 1,
  w_statistics      = c(),
  verbose           = TRUE
)

##################################################################################
# Load and execute visualization functions if TRUE.
if (visualisation_of_point_patterns) {
  vis_pp(reconstruction)
}

if (visualisation_of_summary_statistics) {
  plot_sum_stat(reconstruction)
}
