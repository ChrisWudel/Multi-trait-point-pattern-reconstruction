################################################################################
#             Function for reconstructing marked point patterns                #               
#                  with two marks (Wudel et al., 2022).                        #
################################################################################### Initialisation of the function parameters.
Pattern_reconstruction_with_two_marks <-function(marked_pattern,                 ## Reference point pattern used for reconstruction.
                                                 n_repetitions     = 1,          ## Number of reconstructions to be carried out.
                                                 max_runs          = 10000,      ## Number of simulation runs.
                                                 no_changes        = 5,          ## Number of iterations (per issue interval) after which the reconstruction is aborted if the energy does not decrease.
                                                 rcount            = 250,        ## Number of intervals for which the summary statistics are evaluated.
                                                 rmax              = 5,          ## Is the maximum interval at which the summary statistics are evaluated.
                                                 issue             = 1000 ,      ## Determines after how many simulation steps an output occurs.
                                                 use.g_func        = TRUE,       ## G-functions are taken into account for energy calculation if "TRUE".
                                                 divisor           = c("d","r"), ## Specifies by which of the smoothing kernels to be divided.
                                                 timing            = FALSE,      ## Measures the process time if this is "TRUE".
                                                 energy_evaluation = FALSE,      ## Stores the energy components of the total energy per simulation step, if this is "TRUE".
                                                 show_graphic      = FALSE,      ## The point patterns are displayed and updated during the reconstruction if this parameter is "TRUE".
                                                 bw                = 0.5)        ## Is the bandwidth with which the kernels are scaled and represents its standard deviation.
{ 
  
################################################################################### If several reconstructions are to be carried out, a list is created here in which the results are then saved continuously.
  if(n_repetitions > 1) {                                                           
  names_reconstruction <- paste0("reconstruction_", seq_len(n_repetitions))
  reconstruction_list <- vector("list", length = n_repetitions)
  names(reconstruction_list) <- names_reconstruction  
}
  
################################################################################### Loop to perform multiple reconstructions.   
for (t in seq_len(n_repetitions)) {
  
################################################################################### Definition of the product-moment function for calculating the contribution of a point at the coordinates x, y with marking.  
  calc_moments <- function(fn, p, exclude=NULL, x, y, mark, kernel, rmax, r) {    
  d2           <- (p$x-x)^2 + (p$y-y)^2
  use          <-  d2 <= rmax^2  
  use[exclude] <- FALSE
  z            <- crossprod(p$mark[use, , drop = FALSE],
                    outer(sqrt(d2[use]), r, function(d, r) kernel(r, d)))
  z[fn$i, , drop = FALSE] * mark[fn$j] + z[fn$j, , drop = FALSE] * mark[fn$i]
}
################################################################################### Definition of the calc_moments_full function to calculate the calc_moments function for the whole pattern.
calc_moments_full <- function(fn, p, kernel, rmax, r) {                           
  f <- 0
  for (i in seq_len(nrow(p))) {
    f <- f + calc_moments(fn, p, i:nrow(p), p$x[i], p$y[i], p$mark[i, ],
                          kernel, rmax, r)  
  } 
  rownames(f) <- paste(names(fn$i), names(fn$j), sep = ":")
  f
}

################################################################################### Defining the Energy_fun function to calculate the "energy" of the pattern (where a lower energy indicates a better match).
Energy_fun <- function(f, f0, G, f_, f0_, G_) {                                   
  energy_gest <- 0
  if (use.g_func) {
    energy_gest <- 1 * mean(abs(G - G_), na.rm = TRUE)
  }
  energy_markcrosscorr   <- sum(fn$w * rowMeans(
    abs(f / rep(f["1:1", ], each = nrow(f)) - f_ / rep(f_["1:1", ],
                                                       each = nrow(f_)))))
  energy_markcrosscorr_0 <- sum(fn$w0 * abs(f0 / f0["1:1"] - f0_ / f0_["1:1"]))
  energy_pcf             <- if (divisor == "r") {
                              mean(abs(f["1:1", ] * (diff(xr) * diff(yr) / 
                                                       (2 * pi * nrow(p)^2)) -
                              f_["1:1", ] * (diff(xr) * diff(yr) / 
                                               (2 * pi * nrow(p_)^2))) / r)
                            } else {
                              mean(abs(f["1:1", ] * (diff(xr) * diff(yr) / 
                                                       (2 * pi * nrow(p)^2)) -
                                         f_["1:1", ] * (diff(xr) * diff(yr) /
                                                       (2 * pi * nrow(p_)^2))))  
                            }
c(energy = energy_markcrosscorr + 
           energy_markcrosscorr_0 + 
           energy_pcf + 
           energy_gest,                                                           
           energy_markcrosscorr           = energy_markcrosscorr,
           energy_markcrosscorr_0         = energy_markcrosscorr_0,
           energy_pcf                     = energy_pcf, 
           if (use.g_func) {c(energy_gest = energy_gest)})
}

################################################################################### Load the reference point pattern as a data frame with the components x, y, mark, where x, y are the coordinates of the point and mark is a matrix representing the marks or their dummy values.                                                                    
p_ <- data.frame(       
        x = marked_pattern$x,
        y = marked_pattern$y)
 
p_$mark <- cbind(`1`= 1,
                 diameter = .001 * marked_pattern$marks[[1]],
                 outer(marked_pattern$marks[[2]], 
                 structure(levels(marked_pattern$marks[[2]]),
                 names    = paste0("species", 
                                   levels(marked_pattern$marks[[2]]))), `==`))
      
xr <-marked_pattern$window$xrange 
yr <- marked_pattern$window$yrange  

################################################################################### Check whether certain requirements are met; if not, the reconstruction is aborted and an error message is displayed.    
if (is.marked(spatstat.geom::is.marked(marked_pattern))) {                       
  stop("'marked_pattern' must be marked", call. = FALSE)
}

if (class(marked_pattern[["marks"]][[1]]) != "numeric") {
  stop("mark one must be 'numeric', an example would be the DBH 
       (Diameter at breast height).", call. = FALSE)
}

if (class(marked_pattern[["marks"]][[2]]) != "factor") {
  stop("mark two must be a 'factor', an example would be the tree species.",
       call. = FALSE)
}

if (n_repetitions < 1) {
  stop("n_repetitions must be at least 1 for the function to be executed.",
       call. = FALSE)
}

if(!(divisor == "d"||divisor == "r")) {                                          
  stop("the dievisor must be d or r.", call. = FALSE)
}  

################################################################################### Definition of parameters for the estimation of correlation functions.
rmin    <- rmax / rcount
r       <- seq(rmin, rmax, , rcount)   
rmax_bw <- rmax + 3 * bw

################################################################################### Calculation of the kernels.
kernel <-if(divisor=="d") {                                                       
           function(r, d) pmax(0, (1 - ((r - d) / bw)^2 / 5) * 0.75 / (d * bw))  ## Epanechnikov kernel
} else {
  function(r, d) pmax(0, (1 - ((r - d) / bw)^2 / 5) * 0.75 / (bw * sqrt(5)))
} 

################################################################################### If the G-function for energy calculation is to be taken into account, the package "spatstat" is loaded here and an error message is displayed if it is not installed.
if (use.g_func) require (spatstat)                                                

if("spatstat" %in% rownames(installed.packages()) == FALSE) {
  stop("the package 'spatstat' must be installed on your computer for the 
       application.", call. = FALSE)
} 

################################################################################### Variable definition for the calculation of the mark correlation functions.
marknames <- colnames(p_$mark)
diameter  <- marknames[2]
species   <- marknames[-(1:2)]
energy    <- c() 
fn        <- list()

################################################################################### Determination of the weightings of the mark correlation functions.
for (i in seq_along(marknames)) for (j in seq_along(marknames)) if (i <= j) {     
  fn$i <- c(fn$i,i)
  fn$j <- c(fn$j,j)
  fn$w <- c(fn$w,
    if (marknames[i]     == diameter && marknames[j] == diameter) 0.1
    else if(marknames[i] == "1" || marknames[j]      =="1") 0.5
    else if(marknames[i] == diameter || marknames[j] == diameter) 1
    else 0.2)
  
  fn$w0<-c(fn$w0,
    if (marknames[i]     == diameter && marknames[j] == diameter) 0.1
    else if(marknames[i] == "1" || marknames[j]      == "1") 0.5
    else if(marknames[i] == diameter || marknames[j] == diameter) 2
    else 2)
}

names(fn$i) <- marknames[fn$i]
names(fn$j) <- marknames[fn$j]

################################################################################### Defines the initial state of the new dot pattern.
p   <- p_[sample.int(nrow(p_),nrow(p_), replace = TRUE), ]    
p$x <- runif(nrow(p_), xr[1], xr[2])
p$y <- runif(nrow(p_), yr[1], yr[2])

p$mark[, diameter] <- quantile(p_$mark[, diameter], 
                               probs = runif(nrow(p_), 0, 1), type = 4)            

p$mark[, species] <- p_$mark[, species, drop = FALSE][
  sample.int(nrow(p_), ,replace = TRUE),, drop = FALSE]

################################################################################### Calculates the functions for the reference and the new dot pattern as well as calculating the "energy" that measures their distance.
f_         <- calc_moments_full(fn, p_, kernel, rmax_bw, r)
f0_        <- colSums(p_$mark[, fn$i] * p_$mark[, fn$j])
names(f0_) <- rownames(f_)
f          <- calc_moments_full(fn, p, kernel, rmax_bw, r)
f0         <- colSums(p$mark[, fn$i] * p$mark[, fn$j])
names(f0)  <- rownames(f) 

################################################################################### Calculation of the G-function, if this is to be taken into account for the energy calculation.
if (use.g_func) {
  nnd_ <- nndist(p_$x, p_$y)
  G_   <- cumsum(hist(nnd_[nnd_ <= rmax], breaks = c(-Inf, r), 
                      plot = FALSE) $ count) / length(nnd_)
  nnd  <- nndist(p$x, p$y)
  G    <- cumsum(hist(nnd[nnd <= rmax], breaks=c(-Inf, r), 
                      plot=FALSE)$count) / length(nnd)
} else {
  G_ <- G 
  G <- NULL
}

################################################################################### Show warning if certain distances between pairs of trees are not present.
if(any(f_["1:1", ] == 0)) {  
  warning("Certain distances between pairs of trees are not present in the 
          existing reference pattern.", call. = FALSE)
}
   
Energy_ <- Energy_fun(f, f0, G, f_, f0_, G_)
energy  <- Energy_["energy"]

################################################################################### Prepare the graphical output.
if(show_graphic == TRUE) {                                                        
  par(mfrow = 1:2)
  plot(y~x, p_,  pch=19, col= 2 + mark[, species, drop = FALSE] %*% 
         seq_along(species), cex = 1.3 + 4 * mark[, diameter], xlim = xr, 
       ylim = yr, xaxs ="i", yaxs ="i", main ="Reference", xlab ="x [m]",
       ylab ="y [m]")
  text(p_$x, p_$y, p_$mark[, species, drop = FALSE] %*% seq_along(species), 
       cex=0.7)

  plot(y~x, p, type = "n",
       xlim = xr, ylim = yr, xaxs = "i", yaxs = "i", main = "Reconstructed",
       xlab = "x [m]", ylab = "y [m]")
  clip(xr[1], xr[2], yr[1], yr[2])
}

################################################################################## Prepares variables for the storage of progress.
energy_launch            <- as.vector(energy)
energy_course            <- data.frame(i = seq(from = 1, to = max_runs,by = 1),
                                       energy = NA)
no_changes_energy        <- data.frame(energy = NA)
move_coordinate          <- 0
switch_coords            <- 0
pick_mark_one          <- 0
pick_mark                <- 0
pick_mark_two             <- 0
Exchange_mark_one        <- 0
Exchange_mark_two        <- 0
energy_improvement       <- 0
move_coordinate_improv   <- 0
switch_coords_improv     <- 0
pick_mark_one_improv   <- 0
pick_mark_improv         <- 0
pick_mark_two_improv      <- 0
Exchange_mark_one_improv <- 0
Exchange_mark_two_improv <- 0
no_changes_counter       <- 0

################################################################################### loop to improve the newly created dot pattern by reducing its energy.
l <- 0
system.time(repeat {
  energy_course[l, 2] <- energy
  
################################################################################## Updating the graphical output of all "issue" steps.  
if (l %% issue  == 0) {
  if(show_graphic == TRUE) {
    rect(xr[1], yr[1], xr[2], yr[2], col="white")
    points(y~x, p, pch = 19, col = 2 + mark[, species, drop = FALSE] %*% 
             seq_along(species), cex = 1.3 + 4 * mark[, diameter])
    text(p$x, p$y,p$mark[, species, drop = FALSE] %*% seq_along(species), 
         cex = 0.7)
}
  
################################################################################### Generates text output with the current calculated values (for example the energy), this is updated every "issue" simulation step.  
if(n_repetitions > 1) {
  message("\r> Progress:", names_reconstruction[[t]], " || iterations: ", l,
          " || Simulation progress: ", floor(l/max_runs * 100), "%",
          " || energy = ", round(energy, 5), " || energy improvement = ",
          energy_improvement, "\t\t", appendLF = FALSE)
   } else {
     message("\r> Progress: iterations: ",l," || Simulation progress: ", 
             floor(l/max_runs * 100), "%", " || energy = ",round(energy, 5),
             " || energy improvement  = ",energy_improvement,"\t\t", 
             appendLF = FALSE)
}
  
################################################################################### the next code block aborts the reconstruction if the energy does not decrease in "no_changes" intervals of 1000 simulation steps.
no_changes_energy[l, ] <- rbind(energy)
if (l > 1000) {
  if(round(no_changes_energy[l - 1000, ], 5) == round(energy, 5)) {   
    no_changes_counter <- no_changes_counter + 1
    if(no_changes_counter == no_changes) {
      message("the simulation was terminated, because the energy did not  
              decrease in ", no_changes * 1000, " simulation steps.")
      stop_criterion <- "no_changes"
      break   
    }
  } else {
    no_changes_counter<-0
    stop_criterion<-"max_runs"
    }
}

Sys.sleep(0)
flush.console()
}

if (l < max_runs) l <- l + 1 else break
action <- sample(c("move_coordinate", "switch_coords", "pick_mark_one", 
                   "pick_mark_two", "Exchange_mark_one", "Exchange_mark_two"), 
                   1,, c(.4, .1, .1,.1,.2,.1))
if (use.g_func) H <- G

################################################################################### Switch selection for the possible changes to the reconstructed point pattern for energy minimisation (probabilities of how often an action is taken: 40%, 10%, 20%, 10%, 10%).
switch(action,
       move_coordinate = {                                                       ## Displacement of coordinates of a point in the new point pattern, is applied in 40% of the cases.
         move_coordinate <- move_coordinate + 1
                   i     <- sample.int(nrow(p), 1, replace = TRUE)
                   s     <-  nrow(p) * 1 / (l)                                                     
                   x     <- rnorm(1, p$x[i], diff(xr) *s) %% xr[2]
                   y     <- rnorm(1, p$y[i], diff(yr) * s) %% yr[2]
                   mdiff <- p$mark[i, ]
         
                   g     <- f - calc_moments(fn, p, i, p$x[i], p$y[i], mdiff, 
                                             kernel, rmax_bw, r) + 
                                calc_moments(fn, p, i, x, y, mdiff, kernel,
                                             rmax_bw, r)
                   g0    <- f0 
                   if (use.g_func) {
                     nnd <- nndist(replace(p$x, i, x), replace(p$y, i, y))
                     H   <- cumsum(hist(nnd[nnd <= rmax], breaks=c(-Inf, r),
                                        plot = FALSE)$count) / length(nnd)
                   }
       },
       switch_coords = {                                                         ## Swaps the coordinates of two randomly drawn points from the new point pattern, applied in 10% of the trap.
         switch_coords <- switch_coords + 1
         i             <- sample.int(nrow(p), 2, replace = FALSE)
         mdiff         <- p$mark[i[1], ] - p$mark[i[2], ]
         g             <- f - calc_moments(fn, p, i, p$x[i[1]], p$y[i[1]], 
                                           mdiff, 
                                           kernel, rmax_bw, r) + 
                                           calc_moments(fn, p, i, p$x[i[2]],
                                                        p$y[i[2]], mdiff,
                                                        kernel, rmax_bw, r)
         g0<- f0 
       },
       Exchange_mark_one = {                                                     ## Displacement of coordinates of a point in the new point pattern, applied in 40% of the cases.
         Exchange_mark_one <- Exchange_mark_one + 1
         i                 <- sample.int(nrow(p), 2, replace = FALSE)
         m                 <- p$mark[i, ]
         m[, diameter]     <- m[2:1, diameter]
         mdiff             <- m[1, ] - p$mark[i[1], ]
         q                 <- p[i, ]
         q$mark[1, ]       <- m[1, ]
      
         g                 <- f  + calc_moments(fn, p, i[1], p$x[i[1]], 
                                                p$y[i[1]], mdiff, kernel,
                                                rmax_bw, r) -
                                   calc_moments(fn, p, i, p$x[i[2]], p$y[i[2]], 
                                                mdiff, kernel, rmax_bw, r) -
                                   calc_moments(fn, q, 2, q$x[2], q$y[2], mdiff,
                                               kernel, rmax_bw, r)
         g0                <- f0 + m[1,fn$i] * m[1, fn$j] - p$mark[i[1], fn$i] *
                                   p$mark[i[1], fn$j] + m[2, fn$i] *
                                   m[2, fn$j] - p$mark[i[2], fn$i] * 
                                   p$mark[i[2], fn$j]
       },
       Exchange_mark_two = {                                                     ## Swaps the type assignment of two randomly drawn points from the new point pattern, applied in 10% of the trap.
       Exchange_mark_two <- Exchange_mark_two + 1
       i                 <- sample.int(nrow(p), 2, replace = FALSE)
       m                 <- p$mark[i, ]
       m[, species]      <- m[2:1, species]
       mdiff             <- m[1, ] - p$mark[i[1], ]
       q                 <- p[i, ]
       q$mark            <- m
      
       g                 <- f +  calc_moments(fn, p, i[1], p$x[i[1]], p$y[i[1]],
                                             mdiff, kernel, rmax_bw, r) -
                                 calc_moments(fn, p, i, p$x[i[2]], p$y[i[2]], 
                                             mdiff, kernel, rmax_bw, r) -
                                 calc_moments(fn, q, 2, q$x[2], q$y[2], mdiff, 
                                            kernel, rmax_bw, r)
       g0                <- f0 + m[1, fn$i] * m[1, fn$j] - p$mark[i[1], fn$i] * 
                                 p$mark[i[1], fn$j] + m[2, fn$i] * m[2, fn$j] - 
                                 p$mark[i[2], fn$i] * p$mark[i[2], fn$j]
       },
       pick_mark_one = {                                                         ## If the distribution (continuous function) of the diameter of the reference pattern generates a randomly drawn value for a randomly selected point in the new point pattern, the trap is applied in 20%.
         pick_mark_one <- pick_mark_one + 1
                   i     <- sample.int(nrow(p), 1, replace = TRUE)  
                   m     <- p$mark[i, ]
             m[diameter] <-quantile(p_$mark[,diameter],probs = runif(1,0,1),
                                    type = 4)                                         
                   mdiff <- m - p$mark[i, ] 
                   g     <- f + calc_moments(fn, p, i, p$x[i], p$y[i], mdiff, 
                                             kernel, rmax_bw, r)
                   g0    <- f0 + m[fn$i] * m[fn$j] - p$mark[i, fn$i] * 
                              p$mark[i, fn$j]
    },
    pick_mark_two = {                                                            ## Draws a random value for a point from the new point pattern from the type distribution (discrete function) of the reference pattern, is applied in 10% of the traps.                                                             
      pick_mark_two<-pick_mark_two+1
      i          <- sample.int(nrow(p), 1, replace = TRUE)
      j          <- sample.int(nrow(p_), 1, replace = TRUE)
      m          <- p$mark[i, ]
      m[species] <- p_$mark[j, species]
      mdiff      <- m - p$mark[i, ]
      g          <- f + calc_moments(fn, p, i, p$x[i], p$y[i], mdiff, kernel, 
                                     rmax_bw, r)
      g0         <- f0 + m[fn$i] * m[fn$j] - p$mark[i, fn$i] * p$mark[i, fn$j]
    },
    stop("undefined case")
)

Energy <- Energy_fun(g ,g0 ,H ,f_ ,f0_ ,G_)
e<-Energy[["energy"]]
if(e >= energy) next
f  <- g
f0 <- g0
if (use.g_func) G <- H
energy <- e

switch(action,
       move_coordinate = {
         p$x[i] <- x
         p$y[i] <- y
       },
       
       switch_coords = {
         p$x[i] <- p$x[rev(i)]
         p$y[i] <- p$y[rev(i)]
       },
       
       pick_mark_one   =,
       pick_mark         =,
       pick_mark_two      =,
       Exchange_mark_one =,
       Exchange_mark_two = {
         p$mark[i, ] <- m
       },
       stop("undefined case")
)

################################################################################### Saves the intermediate results and increases running numbers.
energy_improvement <- energy_improvement + 1
  
if (energy_evaluation == TRUE) {
   energy_markcrosscorr   <- Energy[["energy_markcrosscorr"]]
   energy_markcrosscorr_0 <- Energy[["energy_markcrosscorr_0"]]
   energy_pcf             <- Energy[["energy_pcf"]]
  if (use.g_func) {
    energy_gest           <- Energy[["energy_gest"]]
  }
  
  if(energy_improvement == 1) {
    energy_list_overall         <-data.frame(l, action, energy)
    energy_list_markcrosscorr   <-data.frame(l, action, energy_markcrosscorr )
    energy_list_markcrosscorr_0 <-data.frame(l, action, energy_markcrosscorr_0)
    energy_list_pcf             <-data.frame(l, action, energy_pcf)
    if (use.g_func) {
      energy_list_gest          <-data.frame(l, action, energy_gest)
    }
  } else {
    energy_list_overall              <-rbind(energy_list_overall, c(l,action, 
                                                                    energy))
    energy_list_markcrosscorr[l, ]   <- rbind(c(l, action, 
                                                energy_markcrosscorr))
    energy_list_markcrosscorr_0[l, ] <- rbind(c(l, action, 
                                                energy_markcrosscorr_0))
    energy_list_pcf[l, ]             <- rbind(c(l, action, energy_pcf))
    if (use.g_func) {
      energy_list_gest[l, ]          <- rbind(c(l, action, energy_gest))
    }
}
 
switch(action,
       move_coordinate = {
         move_coordinate_improv <- move_coordinate_improv + 1
       }, 
       
       switch_coords = {
         switch_coords_improv <- switch_coords_improv + 1
       },
       
       pick_mark_one = {
         pick_mark_one_improv <- pick_mark_one_improv + 1
       }, 
       
       pick_mark_two = {
         pick_mark_two_improv <- pick_mark_two_improv + 1
       },
       
       Exchange_mark_one = {
         Exchange_mark_one_improv <- Exchange_mark_one_improv + 1
       }, 
       
       Exchange_mark_two = {
         Exchange_mark_two_improv <- Exchange_mark_two_improv + 1
       }
)
}

################################################################################### End of reconstruction loop.  
}) -> process.time

################################################################################### Saves all results Transfers them to the "reconstruction" list.
if(energy_evaluation == TRUE) {
  energy_list_overall$energy                         <- 
    as.numeric(energy_list_overall$energy)
  energy_list_markcrosscorr                          <- 
    na.omit(energy_list_markcrosscorr)
  energy_list_markcrosscorr$energy_markcrosscorr     <- 
    as.numeric(energy_list_markcrosscorr$energy_markcrosscorr)
  energy_list_markcrosscorr_0                        <- 
    na.omit(energy_list_markcrosscorr_0)
  energy_list_markcrosscorr_0$energy_markcrosscorr_0 <- 
    as.numeric(energy_list_markcrosscorr_0$energy_markcrosscorr_0)
  energy_list_pcf                                    <- na.omit(energy_list_pcf)
  energy_list_pcf$energy_pcf                         <- 
    as.numeric(energy_list_pcf$energy_pcf)  
  
  if (use.g_func) {
    energy_list_gest                                 <- 
      na.omit(energy_list_gest)
    energy_list_gest$energy_gest                     <-
      as.numeric(energy_list_gest$energy_gest)
  }
}

ppp_reference     <- ppp(p_$x, p_$y, xr, yr, 
                         marks = data.frame(
                                   diameter = p_$mark[, diameter], 
                                   species  = factor(
                                     p_$mark[, species, drop = FALSE] %*% 
                                       seq_along(species), , species)))

ppp_reference$marks$species <- lapply(ppp_reference$marks$species,
                                      as.character)
ppp_reference$marks$species <- gsub("species","",
                                    ppp_reference$marks$species)
ppp_reference$marks$species <- as.factor(ppp_reference$marks$species)
                          
ppp_reconstructed <- ppp(p$x, p$y, xr, yr, 
                         marks = data.frame(
                                   diameter = p$mark[, diameter], 
                                   species  = factor(
                                     p$mark[, species, drop = FALSE]
                                     %*% seq_along(species), ,species)))
                                   
ppp_reconstructed$marks$species <- lapply(ppp_reconstructed$marks$species, 
                                          as.character)
ppp_reconstructed$marks$species <- gsub("species","",
                                        ppp_reconstructed$marks$species)
ppp_reconstructed$marks$species <- as.factor(ppp_reconstructed$marks$species)
  
method            <- "Reconstruction of a homogeneous point pattern"
Parameter         <- c("n_repetitions", "max_runs", "no_changes", "rcount", 
                       "rmax", "issue", "use.g_func", "divisor", "timing",
                       "energy_evaluation", "show_graphic", "bw")

Value             <- c(n_repetitions, max_runs, no_changes, rcount, rmax, issue,
                       use.g_func, divisor, timing, energy_evaluation, 
                       show_graphic, bw) 
Parameter_setting <- data.frame(Parameter, Value)                                   
iterations        <- l
energy_current    <- energy_course[l, 2]

if(energy_evaluation == TRUE) {
  number_of_actions                                    <- 
    data.frame(c("move_coordinate", "switch_coords", "pick_mark_one", 
                 "pick_mark_two", "Exchange_mark_one", "Exchange_mark_two"),
               c(move_coordinate, switch_coords, pick_mark_one, pick_mark,
                 Exchange_mark_one, Exchange_mark_two))                            
  
  colnames(number_of_actions)                          <- c("name","value")
   
  number_of_actions_with_energy_improvement            <-
     data.frame(c("move_coordinate", "switch_coords", "pick_mark_one", 
                  "pick_mark_two", "Exchange_mark_one", "Exchange_mark_two"), 
                c(move_coordinate_improv, switch_coords_improv, 
                  pick_mark_one_improv, pick_mark_two_improv, 
                  Exchange_mark_one_improv, Exchange_mark_two_improv))
   
  colnames(number_of_actions_with_energy_improvement) <- c("name","value")
   
   energy_total                                        <- 
     list(energy_overall         = energy_list_overall, 
          energy_markcrosscorr   = energy_list_markcrosscorr, 
          energy_markcrosscorr_0 = energy_list_markcrosscorr_0, 
          energy_pcf             = energy_list_pcf,
          energy_gest            = if (use.g_func) {energy_list_gest}) 
}

reconstruction <- 
  list( reference                                 = ppp_reference,
        reconstructed                             = ppp_reconstructed,
        Parameter_setting                         = Parameter_setting,
        method                                    = method,
        stop_criterion                            = stop_criterion,
        iterations                                = l,
        simulation_time                           = 
          if (timing == TRUE) {
            paste(round(process.time[3], 2), "s")
          },
        energy_launch                             = energy_launch,
        energy_course                             = energy_course,
        energy_current                            = energy_current,
        energy_improvement                        = energy_improvement,
        number_of_actions                         = 
          if(energy_evaluation == TRUE) {
            number_of_actions
          },
        number_of_actions_with_energy_improvement = 
          if(energy_evaluation == TRUE) {
            number_of_actions_with_energy_improvement
          },
        energy_total                              = 
          if(energy_evaluation == TRUE) { 
            energy_total
          })

if (!(timing && energy_evaluation)) {
  reconstruction <- reconstruction[-which(sapply(reconstruction, is.null))]
}

################################################################################### Adds the results of further reconstructions to the "reconstruction" list if several are performed. 
if (n_repetitions > 1) {
  reconstruction_list[[t]] <- reconstruction
}

}

if(n_repetitions > 1) {
  reconstruction_list
  } else {
    reconstruction
  }
  
}
