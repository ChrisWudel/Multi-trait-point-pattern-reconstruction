################################################################################
#         Multi-trait point pattern reconstruction of plant ecosystems         #               
################################################################################
# 
# VERSION: 2.0.0
# DATE:    2022-Today
# @author: Chris Wudel (chris.wudel@tu-dresden.de)
# @author: Robert Schlicht (robert.schlicht@tu-dresden.de)
#
# DESCRIPTION:
# This script implements an algorithm for the structural reconstruction of 
# marked point patterns (e.g., forest ecosystems). Version 2.0.0 is a major 
# update that fixes previous bugs and introduces a fully generic framework. 
# It can now handle an arbitrary number of traits (marks) simultaneously, 
# whether they are continuous (numeric) or categorical (factors).
#
# IMPROVEMENTS IN VERSION 2.0.0:
# - Fully generic trait handling (no limit on the number of marks).
# - Structural extensions for improved scalability and modularity.
# - Bug fixes in the energy evaluation and moment computation logic.
# - Clean separation between data preparation and core algorithm.
#
# MAIN REFERENCE (Application & Workflow):
# Wudel, C., Schlicht, R., Meyer, N., Huth, F., Wehnert, A., Micheletti, T., 
# & Berger, U. (2026). A novel workflow for forest regeneration prediction. 
# European Journal of Forest Research, 145:47. 
# DOI: https://doi.org/10.1007/s10342-026-01886-6
#
# THEORETICAL BACKGROUND (Algorithm & Principles):
# Wudel, C., Schlicht, R., & Berger, U. (2023). Multi-trait point pattern 
# reconstruction of plant ecosystems. Methods in Ecology and Evolution, 
# 14(10), 2668–2679. DOI: https://doi.org/10.1111/2041-210X.14206
#
# DATA SOURCE:
# The datasets used in the study are available via Open Access on Zenodo:
# Archive: https://zenodo.org/records/17775566
#
################################################################################

# --- APPLICATION EXAMPLE ------------------------------------------------------

# The reconstruction algorithm proceeds by first loading the original point 
# pattern (here shown for rostock1) that contains both the full data in the 
# reference plot and overstory information for the entire sample area:
#
# rostock1 <- with(read.csv("rostock1.csv"), list(
#   core_wnd = list(min=c(35,35), max=c(65,65)),
#   wnd = list(min=c(0,0), max=c(100,100)),
#   points = cbind(x=x, y=y),
#   marks = list(
#     dbh = 0.001*dbh..mm.,
#     species = species
#   )
# ))
#
# The dbh variable has been scaled so as to have values of the order of 1 or less 
# (like the other variables), which fits to the default weights assigned in the 
# construct() function below. The reconstruction shown in Figure 4b of the main 
# text is then generated as follows:
#
# set.seed(0)
# pattern <- construct_pattern(
#   rostock1,
#   fixed            = function(p) p$marks[,"dbh"] > 0.1 |
#                        in_window(p$points, rostock1$core_wnd), 
#   edge_correction  = edge_correction_OhserStoyan,
#   progress_stop    = function(p, step, stat, ...) step >= 250000
# )
# with(pattern, plot(points, col=marks$species, cex=10*marks$dbh))

# All functions used here are defined in the R script included below, which 
# would typically be loaded into R with the source() function before applying 
# the preceding code.


# --- WRAPPER FUNCTION ---------------------------------------------------------

# The function construct_pattern() is a wrapper of construct() below 
# for generating a new point pattern from a given pattern. The difference is 
# in the representation of the marks in the input and result objects, where 
# the marks here appear as lists. The function
#   (1) prepares the marks matrix in the form required by construct() by 
#       adding an additional mark equal to 1 at the beginning and converting 
#       categorical marks to dummy variables,
#   (2) calls construct() for the main work, and
#   (3) generates the result object, with a list of marks all appearing as 
#       numeric vectors or matrices or as factors.
# The function has the following arguments:
#   marked pattern   The original point pattern, represented by a list of at 
#                    least two components named "points" (the matrix of the 
#                    coordinates of the points) and "marks" (a list with 
#                    components representing the marks, each having the same 
#                    number of rows as "points").
#   core_wnd, wnd,   Further Arguments directly passed on to construct() (see 
#     ...            descriptions below).

construct_pattern <- function(
  marked_pattern,
  core_wnd = marked_pattern$core_wnd,
  wnd = marked_pattern$wnd,
  ...
) {

  # Prepare marks matrix of point pattern.
  pattern <- marked_pattern["points"]
  marks <- c(list(`1`=rep.int(1,nrow(pattern$points))),
    lapply(marked_pattern$marks, function(x) {
      if (!is.array(x)) {
        colnms <- levels(x)
        if (is.null(colnms) && is.character(x))
          colnms <- sort.int(unique(x), method="radix")
        if (!is.null(colnms)) {
          x <- diag(length(colnms))[match(x,colnms),,drop=FALSE]
          dimnames(x) <- list(NULL, level=colnms)
        }
      }
      storage.mode(x) <- "double"
      x
    })
  )
  pattern$marks <- do.call(cbind, c(deparse.level=0, marks))
  markindex <- rep.int(seq_along(marks), vapply(marks,NCOL,0L))
  metric <- vapply(marks, function(x) is.null(dimnames(x)[2L]$level), NA) &
    seq_along(marks) > 1L
  marks[] <- seq_along(marks)

  # Apply function construct(), which does the main work.
  p <- construct(pattern, markindex, metric, core_wnd, wnd, ...)

  # Store results in marked_pattern.
  marked_pattern$points <- p$points
  marked_pattern$marks <- lapply(marks[-1L], function(i)
    if (metric[i]) {
      p$marks[, markindex==i]
    } else {
      x <- p$marks[, markindex==i, drop=FALSE]
      f <- as.integer(x %*% seq_len(ncol(x)))
      levels(f) <- colnames(x)
      class(f) <- "factor"
      f
    })

  # Return the modified marked_pattern.
  marked_pattern
}


# --- GEOMETRY UTILITIES -------------------------------------------------------

# The following function returns a vector of logical values specifying whether 
# or not points are contained in a given window. It has the following 
# arguments: 
#   points           A matrix with each row representing the coordinates of 
#                    one point.
#   wnd              A window, specified as a list of two vectors min and max 
#                    containing the lower and and upper endpoints of the 
#                    intervals for each coordinate.

in_window <- function(
  points,
  wnd
) {
  result <- rep.int(TRUE, nrow(points))
  for (k in seq_len(ncol(points))) result <- result &
    wnd$min[k] <= points[,k] & points[,k] < wnd$max[k]
  result  
}


# --- CORE ALGORITHM -----------------------------------------------------------

# The following function construct() generates and returns a new point pattern 
# represented as a list of two matrices named "points" (the coordinates of the 
# points) and "marks" (the marks, with a separate column for each level) with 
# the same number of rows. The function has the following arguments:
#   pattern          The original point pattern, represented by a list of the 
#                    same form as the result object. This pattern has two 
#                    functions:
#                    (1) Inside core_wnd it represents the reference point 
#                        pattern.
#                    (2) It provides fixed points for the new pattern (which 
#                        is constructed in wnd) as determined by fixed().
#   markindex        An integer vector with a length equal to the number of 
#                    columns of pattern$marks. Columns of pattern$marks with 
#                    the same markindex represent (different levels of) the 
#                    same mark and are always modified as a unit.
#   metric           A logical vector to be indexed by the values of 
#                    markindex (i.e., with a single element representing all 
#                    levels of a mark), indicating which marks have value on a 
#                    continuous scale. This affects the generation of new mark 
#                    values in the draw_mark and add_point actions.
#   core_wnd         The window that the original pattern restricted to 
#                    defines the reference pattern, in the form described for 
#                    in_window().
#   wnd              The window in which the new pattern is constructed, in 
#                    the form described for in_window().
#   fixed            A function with a single argument (a list having with 
#                    points and marks components like pattern) that returns a 
#                    vector of logical values that are TRUE for points that
#                    (1) should be copied from the reference pattern into the 
#                        new pattern,
#                    (2) should not be newly generated in the new pattern.
#   r                The vector of distances at which the summary statistics 
#                    are computed.
#   kernel           The kernel for smoothing summary statistics as returned 
#                    by select_kernel().
#   edge_correction  A function that returns a scaling factor for all summary 
#                    statistics a each value of r in a given window. The 
#                    default is no edge correction.
#   weights_markcorr A function to which metric is passed as an argument, 
#                    returning a 2-by-k-by-k array specifying the weights 
#                    assigned to each point-pair and local summary statistic.
#                    Here k is the length of metric (i.e., the same  weight is 
#                    assumed for different levels of the same mark.) For 
#                    distinct marks m1 and m2, the average of the values 
#                    provided at indices (m1,m2) and (m2,m1) applies.
#   power            The power (as in Lp distances) applied to the absolute 
#                    values of the differences of the summary statistics in 
#                    constructed and the references patterns for the energy 
#                    (i.e., distance) computation.
#   probabilities    The probabilities at which the different actions for 
#                    improving the pattern are randomly drawn.
#   move_point_scale The standard deviation in each coordinate of movements in 
#                    the move_point action as a proportion of the window 
#                    extent in that coordinate.
#   progress_stop    A function called each time at the beginning of the main 
#                    loop. It can be used to generate output for progress of 
#                    the construction, and its return value determines if the 
#                    process stops (TRUE) or continues (FALSE).
#   ...              Optional additional arguments passed to progress_stop.

construct <- function(
  pattern,
  markindex,
  metric,
  core_wnd,
  wnd,
  fixed            = function(p) FALSE,
  r                = 1:250 * 0.1,
  kernel           = select_kernel("epanechnikov", bw=0.5, divisor="r"),
  edge_correction  = function(r, wnd) 1,
  weights_markcorr = function(metric) c(0,0) %o% metric %o% metric + 1,
  power            = 1,
  probabilities    = c(move_point=0.3, exchange_coords=0.1, exchange_mark=0.2, 
                       draw_mark=0.2, delete_point=0.1, add_point=0.1),
  move_point_scale = function(n, step) n/step,
  progress_stop    = function(p, step, stat, ...) {
                       if (step %% 1000 == 0) {
                         cat("step:", step, "energy:", stat$e, "points:",
                           num(p), "successes:", stat$success, "of",
                           stat$attempt, "\n");
                         flush.console()
                       }
                       step >= 100*num(p) || step >= 1000 &&
                         (stat$e_initial - stat$e) / step <= 1e-4 * stat$e
                     },
  ...
) {

  # Do some argument validation.
  stopifnot(nrow(pattern$marks)==nrow(pattern$points),
    ncol(pattern$marks)==length(markindex))

  # Define functions for obtaining the number of points in a point pattern, 
  # subsetting a point pattern, combining two point patterns or replacing a 
  # part of a point pattern; these all assume component fixed is a (single-
  # column) matrix, too.
  num <- function(p) nrow(p$points)
  subs <- function(p,i) lapply(p, `[`, i, , drop=FALSE)
  bind <- function(p1,p2) mapply(rbind,
    p1, p2, MoreArgs=list(deparse.level=0), SIMPLIFY=FALSE)
  repl <- function(p,i,p1) mapply(function(x,i,x1) `[<-`(x, i, , x1),
    p, p1, MoreArgs=list(i=i), SIMPLIFY=FALSE)

  # Define function for randomly drawing indices of points in a point 
  # pattern, optionally returning NULL if any of them satisfy the fixed 
  # criterion.
  sample_points <- function(p, size, allow_fixed=FALSE) if (num(p)) {
    i <- sample.int(num(p), size, replace = TRUE)
    if (allow_fixed || !any(p$fixed[i])) i
  }

  # Define function for incremental computation of second-order moments for 
  # pairs of points.
  moments <- function(fn, p, exclude=NULL, point, mark, kernel, supp, r) {
    d2 <- 0
    for (k in seq_len(ncol(p$points))) d2 <- d2 + (p$points[,k]-point[k])^2
    use <- d2 <= supp^2
    use[exclude] <- FALSE
    z <- crossprod(p$marks[use,,drop=FALSE],
      outer(sqrt(d2[use]), r, function(d,r) kernel(r,d)))
    z[fn$i, , drop=FALSE] * mark[fn$j] + z[fn$j, , drop=FALSE] * mark[fn$i]
  }

  # Define fast alternative using equivalent C code (omitted) compiled with 
  # tools::Rcmd("SHLIB epanechnikov.c") for simple r and default kernel 
  # (select_kernel("epanechnikov", bw=0.5, divisor="r")) only:
  moments_Rimpl <- moments
  if (FALSE && missing(kernel) && all.equal(r, r[1]*seq_along(r))) {
    dyn.load("epanechnikov.dll")
    on.exit(dyn.unload("epanechnikov.dll"))
    moments <- function(fn, p, exclude=NULL, point, mark, kernel, supp, r) {
      d2 <- 0
      for (k in seq_len(ncol(p$points))) d2 <- d2 + (p$points[,k]-point[k])^2
      use <- d2 <= supp^2
      use[exclude] <- FALSE
      z <- .Call("wmarksum_epanechnikov_r", rnum=length(r), rstep=r[1],
        d=sqrt(d2[use]), bw=0.5, pmarks=p$marks[use,,drop=FALSE])
      z[fn$i, , drop=FALSE] * mark[fn$j] + z[fn$j, , drop=FALSE] * mark[fn$i]
    }
  }

  # Define function for computing distance of point patterns (i.e., the  
  # "energy") from second-order pair and local moments.
  energy <- function(f, f0, n, wedge, f_, f0_, n_, wedge_, power) {
    if (!n || !n_) return(Inf)
    f <- f * rep.int(wedge, rep.int(nrow(f), length(wedge)))
    f_ <- f_ * rep.int(wedge_, rep.int(nrow(f_), length(wedge_)))
    sum(fn$w * rowMeans(abs(f/n - f_/n_)^power)) +
      sum(fn$w0 * abs(f0/n - f0_/n_)^power)
  }

  # Calculate support for the estimation of correlation functions.
  supp <- attr(kernel,"support")(max(r))

  # Calculate edge correction factors for given distances.
  wedge_ <- edge_correction(r, core_wnd)
  wedge <- edge_correction(r, wnd)

  # Determine weights of the mark correlation functions.
  weights <- weights_markcorr(metric)
  fn <- list(i=integer(0), j=integer(0), w=numeric(0), w0=numeric(0))
  for (i in seq_along(markindex))
  for (j in seq_along(markindex))
  if (i <= j) {
    w <- (weights[,markindex[i],markindex[j]] +
      weights[,markindex[j],markindex[i]]) / 2
    if (all(!w)) next
    fn$i <- c(fn$i, i)
    fn$j <- c(fn$j, j)
    fn$w <- c(fn$w, w[1L])
    fn$w0<- c(fn$w0, w[2L])
  }
  names(fn$i) <- colnames(pattern$marks)[fn$i]
  names(fn$j) <- colnames(pattern$marks)[fn$j]

  # Initialize component fixed of pattern.
  pattern$fixed <- matrix(fixed(pattern), num(pattern))

  # Prepare reference point pattern p_ as pattern restricted to core_wnd.
  p_ <- subs(pattern, in_window(pattern$points, core_wnd))

  # Generate initial state of new pattern p by randomizing data from p_ in 
  # in window and then removing those points that satisfy the fixed 
  # criterion, replacing them with the fixed points from the original pattern.
  p <- subs(p_, sample_points(p_, rpois(1, num(p_) *
    prod(wnd$max-wnd$min) / prod(core_wnd$max-core_wnd$min)), TRUE))
  p$points[] <- runif(length(p$points),
    min=rep(wnd$min,each=num(p)), max=rep(wnd$max,each=num(p)))
  p$fixed[] <- fixed(p)
  p <- bind(subs(p, !p$fixed), subs(pattern, pattern$fixed))

  # Do initial full calculation of point pair moments f_, f for reference and 
  # new patterns p_ and p.
  f_ <- f <- matrix(0, length(fn$w), length(r))
  for (i in seq_along(p_$fixed)) f_ <- f_ + moments(
    fn, p_, i:num(p_), p_$points[i, ], p_$marks[i, ], kernel, supp, r)
  for (i in seq_along(p$fixed)) f <- f + moments(
    fn, p, i:num(p), p$points[i, ], p$marks[i, ], kernel, supp, r)

  # Calculate corresponding local values f0_, f0.
  f0_ <- colSums(p_$marks[, fn$i, drop=FALSE] * p_$marks[, fn$j, drop=FALSE])
  f0 <- colSums(p$marks[, fn$i, drop=FALSE] * p$marks[, fn$j, drop=FALSE])
  rownames(f_) <- rownames(f) <- names(f0_) <- names(f0) <-
    paste(names(fn$i), names(fn$j), sep=":")

  # Computate of initial "energy" (distance of point patterns).
  stat <- list()
  stat$e <- stat$e_initial <-
    energy(f, f0, num(p), wedge, f_, f0_, num(p_), wedge_, power)

  # Prepare variables for the storage of progress.
  stat$attempt <- stat$success <- 
    replace(integer(), names(probabilities), 0)

  # Progressively improve energy value by modifying new pattern in a loop.
  step <- 0L
  while (!progress_stop(p, step, stat, ...)) {

    step <- step + 1L

    # Randomly draw action for improving the new point pattern.
    action <- sample(names(probabilities), 1L, prob=probabilities)
    stat$attempt[action] <- stat$attempt[action] + 1L

    # Compute effect of proposed change.
    n.new <- num(p)
    switch(action,

      # Displacement of coordinates of a point:
      move_point = {
        i <- sample_points(p, 1L); if (is.null(i)) next
        p. <- subs(p, i)
        p.$points[] <- rnorm(length(p.$points),
            mean = p$points[i,]-wnd$min,
            sd = (wnd$max-wnd$min) * move_point_scale(num(p), step)
          ) %% (wnd$max-wnd$min) + wnd$min
        if (fixed(p.)) next
        mdiff <- p.$marks
        f.new <- f - 
          moments(fn, p, i, p$points[i,], mdiff, kernel, supp, r) +
          moments(fn, p, i, p.$points, mdiff, kernel, supp, r)
        f0.new <- f0 
      },

      # Swapping the coordinates of two randomly drawn points:
      exchange_coords = {
        i <- sample_points(p, 2L); if (is.null(i)) next
        p. <- subs(p, i)
        p.$points <- p.$points[2:1,,drop=FALSE]
        if (any(fixed(p.))) next
        mdiff <- p.$marks[1, ] - p.$marks[2, ]
        f.new <- f -
          moments(fn, p, i, p.$points[2,], mdiff, kernel, supp, r) +
          moments(fn, p, i, p.$points[1,], mdiff, kernel, supp, r)
        f0.new <- f0
      },

      # Exchanging one of the marks for two randomly chosen points:
      exchange_mark = {
        i <- sample_points(p, 2L); if (is.null(i)) next
        p. <- subs(p, i)
        m <- markindex == sample.int(length(metric), 1L)
        p.$marks[, m] <- p.$marks[2:1, m, drop=FALSE]
        if (any(fixed(p.))) next
        mdiff <- p.$marks[1, ] - p$marks[i[1], ]
        f.new <- f +
          moments(fn, p, i[1], p.$points[1,], mdiff, kernel, supp, r) -
          moments(fn, p., 2, p.$points[2,], mdiff, kernel, supp, r) -
          moments(fn, p, i, p.$points[2,], mdiff, kernel, supp, r)
        f0.new <- f0 +
          p.$marks[1,fn$i] * p.$marks[1,fn$j] -
            p$marks[i[1],fn$i] * p$marks[i[1],fn$j] +
          p.$marks[2,fn$i] * p.$marks[2,fn$j] -
            p$marks[i[2],fn$i] * p$marks[i[2],fn$j]
      },

      # Generating new value from reference pattern for one of the marks:
      draw_mark = {
        i <- sample_points(p, 1L); if (is.null(i)) next
        p. <- subs(p, i)
        m <- markindex == (mi <- sample.int(length(metric), 1L))
        if (metric[mi]) {
          if (!num(p_)) next
          for (m in seq_along(m)[m])
            p.$marks[,m] <- quantile(p_$marks[,m], probs=runif(1,0,1), type=4) 
        } else {
          j <- sample_points(p_, 1L, TRUE); if (is.null(j)) next
          p.$marks[,m] <- p_$marks[j,m]
        }
        if (fixed(p.)) next
        mdiff <- p.$marks - p$marks[i, ] 
        f.new <- f +
          moments(fn, p, i, p.$points, mdiff, kernel, supp, r)
        f0.new <- f0 + 
          p.$marks[fn$i]*p.$marks[fn$j] - p$marks[i,fn$i] * p$marks[i,fn$j]
      },

      # Deletion of a random point:
      delete_point = {
        i <- sample_points(p, 1L); if (is.null(i)) next
        p. <- subs(p, i)
        f.new <- f -
          moments(fn, p, i, p.$points, p.$marks, kernel, supp, r) 
        f0.new <- f0 -
          p.$marks[,fn$i] * p.$marks[,fn$j] 
        n.new <- n.new - 1
      },

      # Addition of a new point:
      add_point = {
        p. <- subs(p_, sample_points(p_, 1L, TRUE)); if (!num(p.)) next
        p.$points[] <- runif(length(p.$points), min=wnd$min, max=wnd$max)
        for (m in seq_along(markindex)[metric[markindex]])
          p.$marks[, m] <- quantile(p_$marks[, m], probs=runif(1,0,1), type=4)
        p.$fixed <- fixed(p.)
        if (p.$fixed) next
        f.new <- f +
          moments(fn, p, , p.$points, p.$marks, kernel, supp, r) 
        f0.new <- f0 + p.$marks[,fn$i] * p.$marks[,fn$j] 
        n.new <- n.new + 1
      },

      # Undefined cases (error):
      stop("undefined case")
    )

    # Calculate the energy for the proposed change.
    e.new <- energy(f.new, f0.new, n.new, wedge,
      f_, f0_, num(p_), wedge_, power)

    # Discard proposed pattern if the energy increased.
    if (e.new > stat$e) next

    # Execute the proposed action and updates statistics.
    stat$success[action] <- stat$success[action] + 1L
    p <- switch(action,
      delete_point = subs(p, -i),
      add_point    = bind(p, p.),
                     repl(p, i, p.)
    )
    f  <- f.new
    f0 <- f0.new
    stat$e <- e.new

    # (End of construction loop)
  }

  # Return the new point pattern.
  p$fixed <- NULL
  p
}


# --- STATISTICAL UTILITIES ----------------------------------------------------

# The following function implements the edge correction for rectangular 
# windows from Ohser & Stoyan (1981), https://doi.org/10.1002/bimj.4710230602
# in 2 dimensions. 

edge_correction_OhserStoyan <- function(r, wnd) {
  rx <- r/(wnd$max[1L]-wnd$min[1L]); sx <- 1/pmax(1,rx)
  ry <- r/(wnd$max[2L]-wnd$min[2L]); sy <- 1/pmax(1,ry)
  pi/2 / pmax(0, asin(sy)-acos(sx) +
    (sqrt(1-sy^2)-sx)*ry + (sqrt(1-sx^2)-sy)*rx + (sx^2+sy^2-1)*rx*ry/2)
}


# The following function returns a kernel for smoothing applied in the 
# computation of the summary statistics.

select_kernel <- function(name, bw, divisor) {
  kernel <- switch(name,
    epanechnikov = {
      a <- bw * sqrt(5)
      support <- function(r) r + a
      switch(divisor,
        {
          support <- function(r) sqrt(r^2 + a/pi)
          function(r, d) pmax.int(0, 1 - ((r^2-d^2)*pi/a)^2) * 0.75/a
        },
        none = function(r, d) pmax.int(0, 1 - ((r-d)/a)^2) * 0.75/a,
        r = function(r, d) pmax.int(0, 1 - ((r-d)/a)^2) * 0.75/(a*2*pi*r),
        d = function(r, d) pmax.int(0, 1 - ((r-d)/a)^2) * 0.75/(a*2*pi*d)
      )
    },
    rectangular =, box = {
      a <- bw * sqrt(3)
      support <- function(r) r + a
      switch(divisor,
        {
          support <- function(r) sqrt(r^2 + a/pi)
          function(r, d) stats::dunif((r^2-d^2)*pi,-a,+a)
        },
        none = function(r, d) stats::dunif(r,d-a,d+a),
        r = function(r, d) stats::dunif(r,d-a,d+a)/(2*pi*r),
        d = function(r, d) stats::dunif(r,d-a,d+a)/(2*pi*d)
      )
    },
    gaussian = {
      support <- function(r) Inf
      switch(divisor,
        function(r, d) stats::dnorm((r^2-d^2)*pi,0,sd=bw),
        none = function(r, d) stats::dnorm(r,d,sd = bw),
        r = function(r, d) stats::dnorm(r,d,sd = bw) / (2*pi*r),
        d = function(r, d) stats::dnorm(r,d,sd = bw) / (2*pi*d)
      )
    },
    cumulative = {
      support <- function(r) r
      switch(divisor,
        function(r, d) as.numeric(d <= r),
        none = function(r, d) as.numeric(d <= r),
        r = function(r, d) (d <= r) / (2*pi*r),
        d = function(r, d) (d <= r) / (2*pi*d)
      )
    }
  )
  attr(kernel, "support") <- support
  kernel
}