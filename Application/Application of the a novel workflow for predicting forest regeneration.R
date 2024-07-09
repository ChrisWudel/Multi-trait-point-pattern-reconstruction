################################################################################
# Application of the point pattern reconstruction with two marks, for the      #
#                          three available data sets.                          #
################################################################################
#install.packages("spatstat")                                                    ## Packages which are necessary for the execution of the point pattern reconstruction, please install if not available.  
library(spatstat)  

#install.packages("ggplot2")                                                     ## Packages required to run the entire script (point pattern reconstruction + visualisation). Please install them if they are not present when you want to run the script.
library(ggplot2)

                                                                                ## Loading function from github.
source("https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/func/reconstruct_pattern_multi.R")
source("https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/func/compute_statistics.R")
source("https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/func/dummy_transf.R")
source("https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/func/energy_fun.R")
source("https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/func/calc_moments.R")
source("https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/func/select_kernel.R")
source("https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/func/plot.rd_multi.R")
source("https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/func/sample_points.R")
source("https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/func/edge_correction.R")

################################################################################## Query whether and which visualisations are to be carried out.
visualisation_of_point_patterns     <- TRUE # or FALSE # the library(ggplot2) and the library(patchwork) must be installed.
################################################################################## Selection of the date set, which is then imported via gihub.
url <-"https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/Records/Real_datasets/Terrestrial_laser_scan_data_340_a31.csv"
data <- read.csv(url,sep = ",", stringsAsFactors= TRUE)
data$dbh <- as.numeric(data$dbh)
################################################################################## Execution of the point pattern reconstruction function.
W <- owin(c(0, 100),c(0, 100))                                                  ## The description of the parameters can be found in the reconstruct_pattern_multi.R function in the func folder.
core_window <- owin(c(35, 65),c(35, 65))

marked_pattern <- as.ppp(data.frame(data), W = W)  
marked_pattern$marks$dbh <- marked_pattern$marks$dbh*0.001  
xr <- marked_pattern$window$xrange
yr <- marked_pattern$window$yrange
obs_window = owin(c(xr),c(yr))

reconstruction <- reconstruct_pattern_multi(
marked_pattern,
fixed_points      = NULL,
edge_correction   = TRUE,     
n_repetitions     = 1,     
max_steps         = 10000,     
no_change         = 5,     
rcount            = 250,     
rmax              = 25,      
issue             = 5000,       
divisor           = "r",    
kernel_arg        = "epanechnikov",
timing            = TRUE,    
energy_evaluation = TRUE,
show_graphic      = FALSE,  
Lp                = 1,      
bw                = 0.5,
sd                = "step",
steps_tol         = 10000,   
tol               = 1e-4,   
w_markcorr        = c(m_m=1000,one_one=1500,  all=1, m_all=1, all_all=1, m_m0=1, one_one0=1, all0=1, m_all0=1, all_all0=1),
prob_of_actions   = c(move_coordinate = 0.3, switch_coords = 0.1, exchange_mark_one = 0.1, exchange_mark_two = 0.1, pick_mark_one     = 0.1, pick_mark_two = 0.1, delete_point = 0.1, add_point = 0.1), 
k                 = 1,       
w_statistics      = c(),              
is.fixed          = function(p) 35 <= p$x & p$x <= 65 & 35 <= p$y & p$y <= 65 | p$mark[,"dbh"] > 0.1,
verbose           = TRUE)   
################################################################################## Loads and executes the function for visualising the point patterns under consideration if TURE. 
if(visualisation_of_point_patterns  == TRUE){ 
  source("https://raw.githubusercontent.com/ChrisWudel/Multi-trait-point-pattern-reconstruction/main/func/vis_patterns.R")
  vis_pp(reconstruction) 
}

