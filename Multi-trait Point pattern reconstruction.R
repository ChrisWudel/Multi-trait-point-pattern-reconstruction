################################################################################
#         Multi-trait point pattern reconstruction of plant ecosystems         #               
################################################################################### Initialisation of the function parameters.
#      @date: 2022-Today
#    @author: chris.wudel@tu-dresden.de
# @co-author: robert.schlicht@tu-dresden.de

Multi_trait_point_pattern_reconstruction <- function(marked_pattern,                 ## Reference point pattern used for reconstruction.
                                                
                                                 xr = marked_pattern$window$xrange,
                                                 yr = marked_pattern$window$yrange,
                                                 obs_window = owin(c(xr),c(yr)),                                                        #obs_window = owin(c(0,800 ), c(0,800))
                                                 
                                                 n_repetitions     = 1,          ## Number of reconstructions to be carried out.
                                                 max_steps         = 10000,      ## Number of simulation runs.
                                                 no_changes        = 5,          ## Number of iterations (per issue interval) after which the reconstruction is aborted if the energy does not decrease.
                                                 rcount            = 250,        ## Number of intervals for which the summary statistics are evaluated.
                                                 rmax              = 25,         ## Is the maximum interval at which the summary statistics are evaluated.
                                                 issue             = 1000 ,      ## Determines after how many simulation steps an output occurs.
                                                 divisor           = "r",        ## Specifies by which of the smoothing kernels to be divided: "none","r", "d" or NULL.
                                                 kernel_arg        = "epanechnikov", ## One of "epanechnikov", "rectangular" (or "box"), "cumulative", "gaussian"  
                                                 timing            = FALSE,      ## Measures the process time if this is "TRUE".
                                                 energy_evaluation = FALSE,      ## Stores the energy components of the total energy per simulation step, if this is "TRUE".
                                                 show_graphic      = FALSE,      ## The point patterns are displayed and updated during the reconstruction if this parameter is "TRUE".
                                                 Lp                = 1,          ## Distance measure for the calculation of the energy function (L_p distance, 1 ≤ p < ∞).
                                                 bw                = if (divisor %in% c("r","d")) 0.5 else 5, ## Bandwidth with which the kernels are scaled, so that this is the standard deviation of the smoothing kernel.
                                                 sd                = "step",
                                                 steps_tol         = 1000,       ## ...
                                                 tol               = 1e-4,       ## tolerance:  the procedure is terminated when the energy change is smaller than 1-tol, this occurs no_changes times.
                                                 w_markcorr        = c(d_d=1, all=1, d_all=1, all_all=1, d_d0=1, all0=1, d_all0=1, all_all0=1), ## Vector of possible weightings of individual mcf's 
                                                 prob_of_actions   = c(move_coordinate = 0.4, switch_coords = 0.1, exchange_mark_one = 0.1, exchange_mark_two = 0.1, pick_mark_one = 0.2, pick_mark_two = 0.1), ## Possible actions: sum to 1(100%).
                                                 k                 = 1,          ## Vector of values k; used only if Dk is included above
                                                      

                                                 w_statistics      = c()        ## A vector of named weights for optional spatial statistics from the package "spatstat" 
                                                                                  ## to be included in the energy computation. This may include:
                                                                                  ##   Dk: distribution function of the distance to the kth nearest neighbor
                                                                                  ##   K:  K_r-functions are taken into account for energy calculation if "TRUE".
                                                                                  ##   Hs: Hs_r-functions are taken into account for energy calculation if "TRUE".
                                                                                  ##    pcf: the "spatstat" pcf-functions of are taken into account for energy calculation if "TRUE".
                                                 ) 
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
  calc_moments <- function(fn, p, exclude=NULL, x, y, mark, kernel, rmax_bw, r) {    
    d2           <- (p$x-x)^2 + (p$y-y)^2
    use          <-  d2 <= rmax_bw^2  
    use[exclude] <- FALSE
    z            <- crossprod(p$mark[use, , drop = FALSE],
                      outer(sqrt(d2[use]), r, function(d, r) kernel(r, d)))
    z[fn$i, , drop = FALSE] * mark[fn$j] + z[fn$j, , drop = FALSE] * mark[fn$i]
  }
  ################################################################################### Definition of the calc_moments_full function to calculate the calc_moments function for the whole pattern.
  calc_moments_full <- function(fn, p, kernel, rmax_bw, r) {                           
    f <- 0
    for (i in seq_len(nrow(p))) {
      f <- f + calc_moments(fn, p, i:nrow(p), p$x[i], p$y[i], p$mark[i, ],
                            kernel, rmax_bw, r)  
    } 
    rownames(f) <- paste(names(fn$i), names(fn$j), sep = ":")
    f
  }
  ################################################################################### Function for the transformation of variables to dummy variables and back
  to.dummy <- function(f) {
    x <- matrix(0, length(f), nlevels(f), dimnames=list(names(f), levels(f)))
    x[cbind(seq_along(f), as.integer(f))] <- 1
    x
    }
  from.dummy <- function(x, levels=colnames(x)) {
    f <- as.integer(x %*% seq_along(levels))
    levels(f) <- levels
    class(f) <- "factor"
    f
    }
  ################################################################################### Compute optional spatial statistics using the spatstat package.
  compute_statistics <- function(x, y, k, xr, yr) {
    stat <- names(w_statistics)
    names(stat) <- stat
    lapply(stat, function(name) switch(name,
      ################################################################################### Calculation of the Dk(r)-function, if this is to be taken into account for the energy calculation.
      Dk = {
        nnd_ <- as.matrix(nndist(x, y, k=k))
        apply(nnd_, 2, function(z) cumsum(hist(z[z <= rmax], breaks = c(-Inf, r), plot = FALSE) $ count) / length(z))  
      },
      ################################################################################### Calculation of the K(r)-function, if this is to be taken into account for the energy calculation.
      K = {
        kest<-Kest(ppp(x,y,window=owin(xr,yr)), rmax=rmax, correction="none")# , breaks = c(-Inf, r) 
        kest$un
      },
      ################################################################################### Calculation of the Hs(r)-function (spherical contact distribution), if this is to be taken into account for the energy calculation.
      pcf = {
        pcfest<-pcf(ppp(x,y,window=owin(xr,yr)), r=c(0,r), kernel=kernel_arg, divisor=divisor, bw=bw, correction="none")
        pcfest$un
      },
      ################################################################################### Calculation of the Hs(r)-function (spherical contact distribution), if this is to be taken into account for the energy calculation.
      Hs = {
        hest<-Hest(ppp(x,y,window=owin(xr,yr)), correction="none") #, breaks = c(-Inf, r)
        hest$raw
      },
      stop("unknown statistic")
    ))
  }

  ################################################################################### Defining the Energy_fun function to calculate the "energy" of the pattern (where a lower energy indicates a better match).
  Energy_fun <- function(f, f0, statistics, f_, f0_, statistics_) {
    result <- c(
      f = sum(fn$w * rowMeans(abs(
        f / nrow(p) -
        f_ / nrow(p_)
      )^Lp)),
      f0 = sum(fn$w0 * abs(
        f0 / nrow(p) - 
        f0_ / nrow(p_)
      )^Lp),
      if (length(w_statistics))
        sapply(seq_along(w_statistics), function(i) w_statistics[i] *
          mean(abs(statistics[[i]] - statistics_[[i]])^Lp, na.rm = TRUE),
          USE.NAMES=FALSE
        )
    )
    c(energy = sum(result), result)
  }
  
  ################################################################################### Load the reference point pattern as a data frame with the components x, y, mark, where x, y are the coordinates of the point and mark is a matrix representing the marks or their dummy values.                                                                    
  p_ <- data.frame(       
          x = marked_pattern$x,
          y = marked_pattern$y)
   
  p_$mark <- cbind(`1`= 1,
                   diameter = marked_pattern$marks[[1]],
                   to.dummy(marked_pattern$marks[[2]])) 

  marknames <- colnames(p_$mark)
  diameter  <- 2
  species   <- seq_along(marknames)[-(1:2)]
  
  ################################################################################### Check whether certain requirements are met; if not, the reconstruction is aborted and an error message is displayed.    
  if (is.null(marked_pattern[["marks"]])) {                       
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
  
  ################################################################################### Definition of parameters for the estimation of correlation functions.
  rmin    <- rmax / rcount
  r       <- seq(rmin, rmax, , rcount)   

  
  ################################################################################### Calculation of the kernels.
  kernel <- switch(kernel_arg,
    epanechnikov = {
      a <- bw * sqrt(5)
      rmax_bw <- rmax + a
      switch(divisor,
        {
          rmax_bw <- sqrt(rmax^2 + a/pi)
          function(r, d) pmax.int(0, 1 - ((r^2-d^2)*pi/a)^2) * 0.75/a
        },
        none = function(r, d) pmax.int(0, 1 - ((r-d)/a)^2) * 0.75/a,
        r = function(r, d) pmax.int(0, 1 - ((r-d)/a)^2) * 0.75/(a*2*pi*r),
        d = function(r, d) pmax.int(0, 1 - ((r-d)/a)^2) * 0.75/(a*2*pi*d)
      )
    },
    rectangular =, box = {
      a <- bw * sqrt(3)
      rmax_bw <- rmax + a
      switch(divisor,
        {
          rmax_bw <- sqrt(rmax^2 + a/pi)
          function(r, d) dunif((r^2-d^2)*pi,-a,+a)
        },
        none = function(r, d) dunif(r,d-a,d+a),
        r = function(r, d) dunif(r,d-a,d+a)/(2*pi*r),
        d = function(r, d) dunif(r,d-a,d+a)/(2*pi*d)
      )
    },
    gaussian = {
      rmax_bw <- Inf
      switch(divisor, 
        function(r, d) dnorm((r^2-d^2)*pi,0,sd=bw),
        none = function(r, d) dnorm(r,d,sd = bw),
        r = function(r, d) dnorm(r,d,sd = bw)/ (2*pi*r),
        d = function(r, d) dnorm(r,d,sd = bw)/ (2*pi*d)
      )
    },
    
    cumulative = {
      rmax_bw <- rmax 
      switch(divisor, 
        function(r, d) as.numeric(d <= r),
        none = function(r, d) as.numeric(d <= r),
        r = function(r, d) (d <= r) / (2*pi*r),
        d = function(r, d) (d <= r) / (2*pi*d)
      )
    }
  )
  
  ################################################################################### If the G-function for energy calculation is to be taken into account, the package "spatstat" is loaded here and an error message is displayed if it is not installed.
  if (length(w_statistics)) {
    if("spatstat" %in% rownames(installed.packages()) == FALSE) {
      stop("the package 'spatstat' must be installed on your computer for the 
           application if the w_statistics option is used.", call. = FALSE)
    }
    require (spatstat)                                                
  }
  
  ################################################################################### Determination of the weightings of the mark correlation functions.
  fn        <- list()
  for (i in seq_along(marknames)) for (j in seq_along(marknames)) if (i <= j) {     
    fn$i <- c(fn$i,i)
    fn$j <- c(fn$j,j)
    fn$w <- c(fn$w,
      if (i     == diameter && j == diameter) w_markcorr["d_d"]            ### parameter für die wichtung einbauen 
      else if(i == 1 || j      ==1) w_markcorr["all"]
      else if(i == diameter || j == diameter) w_markcorr["d_all"]
      else w_markcorr["all_all"])
    fn$w0<-c(fn$w0,
      if (i     == diameter && j == diameter) w_markcorr["d_d0"] 
      else if(i == 1 || j      == 1) w_markcorr["all0"]
      else if(i == diameter || j == diameter) w_markcorr["d_all0"]
      else w_markcorr["all_all0"])
  }
  names(fn$i) <- marknames[fn$i]
  names(fn$j) <- marknames[fn$j]
  
  ################################################################################### Defines the initial state of the new ponit pattern.
  n <- nrow(p_)
  # p   <- p_[sample.int(nrow(p_), n, replace = TRUE), ]    
  xwr <- obs_window$xrange 
  ywr <- obs_window$yrange
  
  p <- p_[sample.int(nrow(p_),ceiling(nrow(p_)*((diff(xwr)*diff(ywr))/(diff(xr)*diff(yr)))),TRUE), ]# rpois 
 
  p$x <- runif(nrow(p), xwr[1], xwr[2])
  p$y <- runif(nrow(p), ywr[1], ywr[2])
  
  p$mark[, diameter] <- quantile(p_$mark[, diameter], 
                                 probs = runif(nrow(p), 0, 1), type = 4)            
  
  p$mark[, species] <- p_$mark[, species, drop = FALSE][
    sample.int(nrow(p_), nrow(p), replace = TRUE),, drop = FALSE]
  
  ################################################################################### Calculates the functions for the reference and the new dot pattern as well as calculating the "energy" that measures their distance.
  f_         <- calc_moments_full(fn, p_, kernel, rmax_bw, r)
  f0_        <- colSums(p_$mark[, fn$i] * p_$mark[, fn$j])
  names(f0_) <- rownames(f_)
  statistics_<- compute_statistics(p_$x, p_$y, k, xr, yr)
  f          <- calc_moments_full(fn, p, kernel, rmax_bw, r)
  f0         <- colSums(p$mark[, fn$i] * p$mark[, fn$j])
  names(f0)  <- rownames(f) 
  statistics <- compute_statistics(p$x, p$y, k, xwr, ywr)

  ################################################################################### Prepare the graphical output.
  if(show_graphic == TRUE) {                                                        
    par(mfrow = 1:2)
    num_species <- from.dummy (p_$mark[, species, drop = FALSE])
    plot(y~x, p_,  pch=19, col= 2L + as.integer(num_species), cex = 1.3 + 4 * mark[, diameter], xlim = xr, 
         ylim = yr, xaxs ="i", yaxs ="i", main ="Reference", xlab ="x [m]",
         ylab ="y [m]")
    text(p_$x, p_$y, as.integer(num_species), cex=0.7)
  
    plot(y~x, p, type = "n",
         xlim = xwr, ylim = ywr, xaxs = "i", yaxs = "i", main = "Reconstructed",
         xlab = "x [m]", ylab = "y [m]")
    clip(xwr[1], xwr[2], ywr[1], ywr[2])
  }
  
  ################################################################################### Show warning if certain distances between pairs of trees are not present.

  energy  <- Energy_fun(f, f0, statistics, f_, f0_, statistics_)["energy"]
  
  ################################################################################## Prepares variables for the storage of progress.
  energy_launch            <- as.vector(energy)
  energy_course            <- data.frame(i = seq(from = 1, to = max_steps,by = 1),
                                         energy = NA)
  energy_improvement       <- 0L
  number_of_actions        <- integer(0)
  number_of_actions_with_energy_improvement <- integer(0)
  no_changes_energy        <- Inf
  no_changes_counter       <- 0L
  step_list                <- integer(0)
  action_list              <- character(0)
  Energy_list              <- numeric(0)

  ################################################################################### loop to improve the newly created dot pattern by reducing its energy.
  step <- 0L
  system.time(repeat {
    energy_course[step, 2] <- energy
    
    ################################################################################## Updating the graphical output of all "issue" steps.  
    if (step %% issue  == 0) {
      if(show_graphic == TRUE) {
        rect(xwr[1], ywr[1], xwr[2], ywr[2], col="white")
        num_species <- from.dummy (p$mark[, species, drop = FALSE])
        
        points(y~x, p, pch = 19, col = 2L + as.integer(num_species), cex = 1.3 + 4 * mark[, diameter])
        text(p$x, p$y, as.integer(num_species), cex = 0.7)
      }
        
      ################################################################################### Generates text output with the current calculated values (for example the energy), this is updated every "issue" simulation step.  
      message("> Progress:", if(n_repetitions > 1) names_reconstruction[[t]], " || iterations: ", step,
              " || Simulation progress: ", floor(step/max_steps * 100), "%",
              " || energy = ", round(energy, 5), " || energy improvement = ",
              energy_improvement, "\t\t\r", appendLF = FALSE)

      Sys.sleep(0)
      flush.console()
    }   
    ################################################################################### the next code block aborts the reconstruction if the energy decreases by less than tol in "no_changes" intervals of steps_tol simulation steps.
    if (step %% steps_tol  == 0) {
      if(energy > no_changes_energy * (1-tol)) {   
        no_changes_counter <- no_changes_counter + 1L
        if(no_changes_counter == no_changes) {
          message("the simulation was terminated, because the energy did not  
                  decrease in ", no_changes * issue, " simulation steps.")
          stop_criterion <- "no_changes"
          break   
        }
      } else {
        no_changes_counter<-0L
        stop_criterion<-"max_steps"
      }
      no_changes_energy <- energy
    }
    
    if (step < max_steps) step <- step + 1 else break
    action <- sample(c("move_coordinate", "switch_coords", "pick_mark_one", 
                       "pick_mark_two", "exchange_mark_one", "exchange_mark_two"), 
                       1,, prob_of_actions)
    number_of_actions[action] <- (if (is.na(number_of_actions[action]))
      0L else number_of_actions[action]) + 1L

    statistics.new <- statistics
    ################################################################################### Switch selection for the possible changes to the reconstructed point pattern for energy minimisation (probabilities of how often an action is taken: 40%, 10%, 20%, 10%, 10%).
    
    switch(action,
           move_coordinate = {                                                       ## Displacement of coordinates of a point in the new point pattern, is applied in 40% of the cases.
                       i     <- sample.int(nrow(p), 1, replace = TRUE)
                       s     <- if(sd=="step") nrow(p) * 1 / step else sd                                     
                       x     <- rnorm(1, p$x[i], diff(xwr) * s) %% xwr[2]
                       y     <- rnorm(1, p$y[i], diff(ywr) * s) %% ywr[2]
                       mdiff <- p$mark[i, ]
             
                       f.new <- f - calc_moments(fn, p, i, p$x[i], p$y[i], mdiff, 
                                                 kernel, rmax_bw, r) + 
                                    calc_moments(fn, p, i, x, y, mdiff, kernel,
                                                 rmax_bw, r)
                       f0.new<- f0 
                       statistics.new <- compute_statistics(replace(p$x, i, x), replace(p$y, i, y), k, xwr, ywr)
           },
           switch_coords = {                                                         ## Swaps the coordinates of two randomly drawn points from the new point pattern, applied in 10% of the trap.
             i             <- sample.int(nrow(p), 2, replace = FALSE)
             mdiff         <- p$mark[i[1], ] - p$mark[i[2], ]
             f.new         <- f - calc_moments(fn, p, i, p$x[i[1]], p$y[i[1]], 
                                               mdiff, 
                                               kernel, rmax_bw, r) + 
                                               calc_moments(fn, p, i, p$x[i[2]],
                                                            p$y[i[2]], mdiff,
                                                            kernel, rmax_bw, r)
             f0.new<- f0 
           },
           exchange_mark_one = {                                                     ## Displacement of coordinates of a point in the new point pattern, applied in 40% of the cases.
             i                 <- sample.int(nrow(p), 2, replace = FALSE)
             m                 <- p$mark[i, ]
             m[, diameter]     <- m[2:1, diameter]
             mdiff             <- m[1, ] - p$mark[i[1], ]
             q                 <- p[i, ]
             q$mark[1, ]       <- m[1, ]
          
             f.new             <- f  + calc_moments(fn, p, i[1], p$x[i[1]], 
                                                    p$y[i[1]], mdiff, kernel,
                                                    rmax_bw, r) -
                                       calc_moments(fn, p, i, p$x[i[2]], p$y[i[2]], 
                                                    mdiff, kernel, rmax_bw, r) -
                                       calc_moments(fn, q, 2, q$x[2], q$y[2], mdiff,
                                                   kernel, rmax_bw, r)
             f0.new            <- f0 + m[1,fn$i] * m[1, fn$j] - p$mark[i[1], fn$i] *
                                       p$mark[i[1], fn$j] + m[2, fn$i] *
                                       m[2, fn$j] - p$mark[i[2], fn$i] * 
                                       p$mark[i[2], fn$j]
           },
           exchange_mark_two = {                                                     ## Swaps the type assignment of two randomly drawn points from the new point pattern, applied in 10% of the trap.
           i                 <- sample.int(nrow(p), 2, replace = FALSE)
           m                 <- p$mark[i, ]
           m[, species]      <- m[2:1, species]
           mdiff             <- m[1, ] - p$mark[i[1], ]
           q                 <- p[i, ]
           q$mark            <- m
          
           f.new             <- f +  calc_moments(fn, p, i[1], p$x[i[1]], p$y[i[1]],
                                                 mdiff, kernel, rmax_bw, r) -
                                     calc_moments(fn, p, i, p$x[i[2]], p$y[i[2]], 
                                                 mdiff, kernel, rmax_bw, r) -
                                     calc_moments(fn, q, 2, q$x[2], q$y[2], mdiff, 
                                                kernel, rmax_bw, r)
           f0.new            <- f0 + m[1, fn$i] * m[1, fn$j] - p$mark[i[1], fn$i] * 
                                     p$mark[i[1], fn$j] + m[2, fn$i] * m[2, fn$j] - 
                                     p$mark[i[2], fn$i] * p$mark[i[2], fn$j]
           },
           pick_mark_one = {                                                         ## If the distribution (continuous function) of the diameter of the reference pattern generates a randomly drawn value for a randomly selected point in the new point pattern, the trap is applied in 20%.
                       i     <- sample.int(nrow(p), 1, replace = TRUE)  
                       m     <- p$mark[i, ]
                 m[diameter] <-quantile(p_$mark[,diameter],probs = runif(1,0,1),
                                        type = 4)                                         
                       mdiff <- m - p$mark[i, ] 
                       f.new <- f + calc_moments(fn, p, i, p$x[i], p$y[i], mdiff, 
                                                 kernel, rmax_bw, r)
                       f0.new<- f0 + m[fn$i] * m[fn$j] - p$mark[i, fn$i] * 
                                  p$mark[i, fn$j]
        },
        pick_mark_two = {                                                            ## Draws a random value for a point from the new point pattern from the type distribution (discrete function) of the reference pattern, is applied in 10% of the traps.                                                             
          i          <- sample.int(nrow(p), 1, replace = TRUE)
          j          <- sample.int(nrow(p_), 1, replace = TRUE)
          m          <- p$mark[i, ]
          m[species] <- p_$mark[j, species]
          mdiff      <- m - p$mark[i, ]
          f.new      <- f + calc_moments(fn, p, i, p$x[i], p$y[i], mdiff, kernel, 
                                         rmax_bw, r)
          f0.new     <- f0 + m[fn$i] * m[fn$j] - p$mark[i, fn$i] * p$mark[i, fn$j]
        },
        stop("undefined case")
    )
    
    Energy <- Energy_fun(f.new, f0.new, statistics.new, f_, f0_, statistics_)
    energy.new<-Energy[["energy"]]
    if(energy.new >= energy) next
    f  <- f.new
    f0 <- f0.new
    statistics <- statistics.new
    energy <- energy.new
    
    switch(action,
           move_coordinate = {
             p$x[i] <- x
             p$y[i] <- y
           },
           
           switch_coords = {
             p$x[i] <- p$x[rev(i)]
             p$y[i] <- p$y[rev(i)]
           },
           
           pick_mark_one     =,
           pick_mark         =,
           pick_mark_two     =,
           exchange_mark_one =,
           exchange_mark_two = {
             p$mark[i, ] <- m
           },
           stop("undefined case")
    )
    
    ################################################################################### Saves the intermediate results and increases running numbers.
    if (energy_evaluation == TRUE) {
      step_list                      <-c(step_list,step)
      action_list                    <-c(action_list, action)
      Energy_list                    <-rbind(Energy_list, Energy)
      number_of_actions_with_energy_improvement[action] <-
        (if (is.na(number_of_actions_with_energy_improvement[action]))
          0L else number_of_actions_with_energy_improvement[action]) + 1L
    }
    energy_improvement <- energy_improvement + 1L
      
  
  ################################################################################### End of reconstruction loop.  
  }) -> process.time

  message("\n")
  ################################################################################### Saves all results Transfers them to the "reconstruction" list.
  p_$species <- from.dummy(p_$mark[, species, drop = FALSE])

  p$species  <- from.dummy(p$mark[, species, drop = FALSE])
    
  method            <- "Reconstruction of a homogeneous point pattern"
  Parameter_setting <- list(n_repetitions=n_repetitions, max_steps=max_steps, no_changes=no_changes,
                         rcount=rcount, rmax=rmax, issue=issue, 
                         divisor=divisor, kernel_arg=kernel_arg, 
                         timing=timing, energy_evaluation=energy_evaluation, show_graphic=show_graphic,
                         Lp=Lp, k=k, bw=bw, sd=sd,prob_of_actions=prob_of_actions, 
                         w_markcorr=w_markcorr, w_statistics=w_statistics) 
  iterations        <- step
  energy_current    <- energy_course[step, 2]

  adapted_p_<- data.frame(p_$x, p_$y, p_$mark[,2], p_$species)
  colnames(adapted_p_)<-c("x", "y", "diameter", "species")
  
  adapted_p<- data.frame(p$x, p$y, p$mark[,2], p$species)
  colnames(adapted_p)<-c("x", "y", "diameter", "species")
  
  win_change<- if (xr[1] != xwr[1] | xr[2] != xwr[2] | yr[1] != ywr[1] | yr[2] != ywr[2]) {TRUE}else{FALSE}
  
  reconstruction <- 
    list( reference                                 = adapted_p_,
          reconstructed                             = adapted_p,
          window                                    = c(xr, yr),
          obs_window                                = 
            if (win_change == TRUE) {
              c(xwr, ywr)
            },
          r                                         = r,
          f_reference                               = f_,
          f_reconstructed                           = f,
          Parameter_setting                         = Parameter_setting,
          method                                    = method,
          stop_criterion                            = stop_criterion,
          iterations                                = step,
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
          energy_details                            = 
            if(energy_evaluation == TRUE) { 
               data.frame(step_list, action_list, Energy_list)
            })
  
  if (!(timing && energy_evaluation && win_change)) {
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

