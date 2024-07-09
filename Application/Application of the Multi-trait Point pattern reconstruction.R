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
source("https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/func/reconstruct_pattern_multi.R")
source("https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/func/compute_statistics.R")
source("https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/func/dummy_transf.R")
source("https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/func/energy_fun.R")
source("https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/func/calc_moments.R")
source("https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/func/select_kernel.R")
source("https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/func/plot.rd_multi.R")
source("https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/func/sample_points.R")

################################################################################## Query whether and which visualisations are to be carried out.
visualisation_of_point_patterns     <- TRUE # or FALSE # the library(ggplot2) and the library(patchwork) must be installed.
visualisation_of_summary_statistics <- TRUE # or FALSE # the library(ggplot2) and the library(reshape) must be installed.

################################################################################## Selection of the date set, which is then imported via gihub.
source("https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/func/select_data.R")

x <- "random" ## The following sets can be imported: 
                                   ## Real datasets:
                                    ##    "VERMOS_project" 
                                    ##    "Northwest_German_Forest_Research_Institute" 
                                    ##    "Marteloscope_data_from_the_by_the_Chair_of_Forest_Growth_and_Woody_Biomass_Production"
                                   ## Simulated patterns:
                                    ##    "random"
                                    ##    "regular"
                                    ##    "cluster_size5"
                                    ##    "cluster_size5_and_random"
                                   ## to do this, declare x with the corresponding name in "". 
data <- data_import(x)
W <- data[[2]] 
data <- data [[1]]

################################################################################## Execution of the point pattern reconstruction function.
marked_pattern <- as.ppp(data.frame(data), W = W)  ### marked_pattern <- as.ppp(data.frame(data$x,data$y,data$`dbh [mm]`,factor(substr(data$`Tree species`,1,4)=="Acer"),data), W = W)  

marked_pattern$marks$dbh..mm.<-marked_pattern$marks$dbh..mm.*0.001              ## Here the metric marker (for these datasets the diameter of the trees)
                                                                                 ## is calculated in the desired unit; for the datasets that can be selected here it is the unit metre [m].

reconstruction <- reconstruct_pattern_multi(
  marked_pattern, 
  n_repetitions     = 1,                                          ## Number of reconstructions to be carried out.
  max_steps         = 100000,                                     ## Number of simulation runs.
  no_change        = 5,                                          ## Number of iterations (per issue interval) after which the reconstruction is aborted if the energy does not decrease.
  rcount            = 250,                                        ## Number of intervals for which the summary statistics are evaluated.
  rmax              = 25,                                         ## Is the maximum interval at which the summary statistics are evaluated.
  issue             = 1000 ,                                      ## Determines after how many simulation steps an output occurs.
  divisor           = "r",                                        ## Specifies by which of the smoothing kernels to be divided: "none","r", "d" or NULL.
  kernel_arg        = "epanechnikov",                             ## One of "epanechnikov", "rectangular" (or "box"), "cumulative", "gaussian"  
  timing            = TRUE,                                       ## Measures the process time if this is "TRUE".
  energy_evaluation = TRUE,                                       ## Stores the energy components of the total energy per simulation step, if this is "TRUE".
  show_graphic      = FALSE,                                      ## The point patterns are displayed and updated during the reconstruction if this parameter is "TRUE".
  Lp                = 1,                                          ## Distance measure for the calculation of the energy function (L_p distance, 1 ≤ p < ∞).
  bw                = 0.5,                                        ## Bandwidth with which the kernels are scaled, so that this is the standard deviation of the smoothing kernel.
  sd                = "step",                                     ## This is the standard deviation used in the move_coordinate action.
  steps_tol         = 1000,                                       ## After the value steps_tol it is checked whether the energy change is smaller than tol.   
  tol               = 1e-4,                                       ## tolerance:  the procedure is terminated when the energy change is smaller than 1-tol, this occurs no_changes times.
  w_markcorr        = c(m_m=1,one_one=0,  all=1, m_all=1,         ## Vector of possible weightings of individual mcf's. (Default: all equal).
                        all_all=1, m_m0=1, one_one0=0, 
                        all0=1, m_all0=1,all_all0=1),

  prob_of_actions   = c(move_coordinate = 0.4,                    ## Possible actions: sum to 1(100%).
                         switch_coords = 0.1,
                         exchange_mark_one = 0.1,
                         exchange_mark_two = 0.1,
                         pick_mark_one = 0.2,
                         pick_mark_two = 0.1,
                         delete_point = 0.0, 
                         add_point = 0.0),
  k                 = 1,                                           ## Vector of values k; used only if Dk is included above
  w_statistics      = c(),                                         ## A vector of named weights for optional spatial statistics from the package "spatstat" 
                                                                   ## to be included in the energy computation. This may include:
                                                                   ##   Dk: distribution function of the distance to the kth nearest neighbor
                                                                   ##   K:  K_r-functions are taken into account for energy calculation if "TRUE".
                                                                   ##   Hs: Hs_r-functions are taken into account for energy calculation if "TRUE".
                                                                   ##   pcf: the "spatstat" pcf-functions of are taken into account for energy calculation if "TRUE".
  verbose           = TRUE                                         ## Logical if progress report is printed.
)  
################################################################################## Loads and executes the function for visualising the point patterns under consideration if TURE. 
if(visualisation_of_point_patterns  == TRUE){ 
  source("https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/func/vis_patterns.R")
  vis_pp(reconstruction) 
}

################################################################################## Loads and executes the summary statistics visualisation function if TURE.
if(visualisation_of_summary_statistics == TRUE){
  source("https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/func/plot_statistics.R")
  plot_sum_stat(reconstruction)
}

