#' plot_sum_stat
#'
#' @description Function to visualise the summary statistics of the point patterns used.
#'
#' @param reconstruction This represents the reconstructed point pattern.
#' 
#' @details
#' The function is used to visualise the results of the point pattern reconstruction 
#' by displaying various statistical functions.
#' 
#' @return void
#'
#' @aliases plot_sum_stat
#' @rdname plot_sum_stat
#'
#' @export
plot_sum_stat <- function(reconstruction) {
  # Install required packages if not already installed
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  if (!requireNamespace("patchwork", quietly = TRUE)) install.packages("patchwork")
  if (!requireNamespace("reshape", quietly = TRUE)) install.packages("reshape")
  
  # Load required libraries
  library(ggplot2)
  library(spatstat)
  library(reshape)

  # Helper function to calculate and format functions
  calculate_functions <- function(ppp, bw, kernel_arg, divisor, r) {
    list(
      k_func = Kest(ppp, correction="none", r = r)$un,
      pcf_func = spatstat.explore::pcf.ppp(ppp, bw = bw, kernel = kernel_arg, correction = "none", divisor = divisor, r = r)$un,
      markcorr_func = markcorr(ppp, correction = "none", r = r)
    )
  }

  # Helper function for plotting
  plot_function <- function(df, title, y_label) {
    ggplot(data = df, aes(x = r, y = value)) +
      geom_line(aes(group = variable), col = "grey") +
      geom_line(data = subset(df, variable == "Reference"), col = "black") +
      geom_line(data = subset(df, variable == paste0(title, "_mean")), col = "black", linetype = "dashed") +
      geom_vline(xintercept = rmax, linetype='dashed') +
      theme_bw() + theme_classic() +
      theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
      ggtitle(title) +
      labs(y = y_label, x = "r [m]") +
      theme(legend.title = element_blank())
  }

  # Initial Setup
  n_repetitions <- if (nchar(names(reconstruction[1])) == 9) 1 else reconstruction[[1]]$Parameter_setting$n_repetitions
  rmax <- as.numeric(reconstruction[[n_repetitions]]$Parameter_setting$rmax)
  rpresented <- min(as.numeric(reconstruction[[n_repetitions]]$window[2]), as.numeric(reconstruction[[n_repetitions]]$window[4]))
  rcount <- as.numeric(reconstruction[[n_repetitions]]$Parameter_setting$rcount)
  r <- seq(0, if (rpresented <= rmax*2) rmax*2 else rpresented, length.out = rcount)
  bw <- as.numeric(reconstruction[[n_repetitions]]$Parameter_setting$bw)
  kernel_arg <- "epanechnikov"
  divisor <- "r"

  # Reference data
  ppp_reference <- as.ppp(reconstruction[[1]]$reference, reconstruction[[1]]$window)

  # Initialize lists for storing results
  k_func_recon <- vector("list", n_repetitions)
  pcf_func_recon <- vector("list", n_repetitions)
  dbh_markcorr_func_recon <- vector("list", n_repetitions)
  species_markcorr_func_recon <- vector("list", n_repetitions)

  # Compute functions for each repetition
  for (i in seq_len(n_repetitions)) {
    ppp_reconstructed <- as.ppp(reconstruction[[i]]$reconstructed, reconstruction[[i]]$window)
    functions <- calculate_functions(ppp_reconstructed, bw, kernel_arg, divisor, r)
    
    k_func_recon[[i]] <- functions$k_func
    pcf_func_recon[[i]] <- functions$pcf_func
    dbh_markcorr_func_recon[[i]] <- functions$markcorr_func[[1]]$un
    species_markcorr_func_recon[[i]] <- functions$markcorr_func[[2]]$un
  }

  # Process and plot K-function
  k_func_recon_df <- as.data.frame(k_func_recon)
  colnames(k_func_recon_df) <- sprintf("k_func%03d", seq_len(n_repetitions))
  k_func <- Kest(ppp_reference, correction="none", r = r)
  k_func_all <- cbind(k_func$un, k_func_recon_df, rowMeans(k_func_recon_df), k_func$r)
  colnames(k_func_all)[1] <- "Reference"
  k_func_all_df <- melt(as.data.frame(k_func_all), "r")
  ggp_k_func_all <- plot_function(k_func_all_df, "K-function", "K(r)")

  # Process and plot Pair Correlation Function (PCF)
  pcf_func_recon_df <- as.data.frame(pcf_func_recon)
  colnames(pcf_func_recon_df) <- sprintf("pcf_func_recon%03d", seq_len(n_repetitions))
  pcf_func <- spatstat.explore::pcf.ppp(ppp_reference, bw = bw, kernel = kernel_arg, correction = "none", r = r, divisor = divisor)
  pcf_func_all <- cbind(pcf_func$un, pcf_func_recon_df, apply(pcf_func_recon_df, 1, min), apply(pcf_func_recon_df, 1, max), rowMeans(pcf_func_recon_df), pcf_func$r)
  colnames(pcf_func_all)[1] <- "Reference"
  pcf_func_all_df <- melt(as.data.frame(pcf_func_all), "r")
  ggp_pcf_func_all <- plot_function(pcf_func_all_df, "Pair correlation function", "g(r)")

  # Process and plot Mark Correlation Functions (DBH and species)
  markcorr_dbh_func <- markcorr(ppp_reference, correction = "none", r = r)[[1]]$un
  markcorr_species_func <- markcorr(ppp_reference, correction = "none", r = r)[[2]]$un

  dbh_markcorr_func_recon_df <- as.data.frame(dbh_markcorr_func_recon)
  colnames(dbh_markcorr_func_recon_df) <- sprintf("dbh_markcorr_func_recon%03d", seq_len(n_repetitions))
  if (n_repetitions > 1) {
    dbh_markcorr_all <- cbind(markcorr_dbh_func, dbh_markcorr_func_recon_df, rowMeans(dbh_markcorr_func_recon_df), markcorr_dbh_func$r)
  } else {
    dbh_markcorr_all <- cbind(markcorr_dbh_func, dbh_markcorr_func_recon_df, markcorr_dbh_func$r)
  }
  colnames(dbh_markcorr_all)[1] <- "Reference mark dbh"
  dbh_markcorr_all_df <- melt(as.data.frame(dbh_markcorr_all), "r")
  ggp_dbh_markcorr_func_all <- plot_function(dbh_markcorr_all_df, "Mark Correlation Function (dbh)", "kmm(r)")

  species_markcorr_func_recon_df <- as.data.frame(species_markcorr_func_recon)
  colnames(species_markcorr_func_recon_df) <- sprintf("species_markcorr_func_recon%03d", seq_len(n_repetitions))
  if (n_repetitions > 1) {
    species_markcorr_all <- cbind(markcorr_species_func, species_markcorr_func_recon_df, rowMeans(species_markcorr_func_recon_df), markcorr_species_func$r)
  } else {
    species_markcorr_all <- cbind(markcorr_species_func, species_markcorr_func_recon_df, markcorr_species_func$r)
  }
  colnames(species_markcorr_all)[1] <- "Reference mark species"
  species_markcorr_all_df <- melt(as.data.frame(species_markcorr_all), "r")
  ggp_species_markcorr_func_all <- plot_function(species_markcorr_all_df, "Mark Correlation Function (species)", "kmm(r)")

  result <- list(
    ggp_k_func_all,
    ggp_pcf_func_all,
    ggp_species_markcorr_func_all,
    ggp_dbh_markcorr_func_all
  )

  cat(sep="\n\n")
  message("look under Plots to see the result.")

  return(result)
}