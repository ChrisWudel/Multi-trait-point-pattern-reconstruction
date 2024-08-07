#' vis_pp
#'
#' @description Function to visualize and compare point patterns.
#' 
#' @param reconstruction This represents the reconstructed point pattern.
#' 
#' @details
#' Function is designed to visualize and compare point patterns from a reconstruction 
#' process. It generates plots for both the reference point pattern and the reconstructed 
#' point pattern, allowing for a visual comparison of the two.
#'
#' @return void
#'
#' @aliases vis_pp
#' @rdname vis_pp
#'
#' @export
vis_pp <- function(reconstruction) {
  # Install required packages if not already installed
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  if (!requireNamespace("patchwork", quietly = TRUE)) install.packages("patchwork")
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  
  library(ggplot2)
  library(patchwork)
  library(dplyr)
  
  # Helper function to create dataframe
  create_ppp_dataframe <- function(ppp_data, Faktor = 1000) {
    df <- data.frame(
      x = ppp_data$x,
      y = ppp_data$y,
      species = ppp_data[3] * Faktor,
      dbh = ppp_data[4]
    )
    df <- df %>%
      mutate(stock_layer = ifelse(dbh > 100, "Upper stand", "Rejuvenation"))
    return(df)
  }

  # Data import from the results of point pattern reconstruction
  if (nchar(names(reconstruction[1])) == 9) {
    ppp_reference <- reconstruction$reference
    ppp_reconstructed <- reconstruction$reconstructed
  } else {
    ppp_reference <- reconstruction$reconstruction_1$reference
    ppp_reconstructed <- reconstruction$reconstruction_1$reconstructed
  }

  # Create dataframes for plotting
  ppp_df_reference <- create_ppp_dataframe(ppp_reference)
  ppp_df_reconstructed <- create_ppp_dataframe(ppp_reconstructed)

  # Plotting function
  create_plot <- function(data, title) {
    ggplot(data, aes(x, y, size = dbh)) +
      geom_point(aes(shape = stock_layer, color = species)) +
      scale_shape_manual(name = "stock layer", 
                         labels = c("Rejuvenation", "Upper stand"), 
                         values = c(13, 16)) +
      scale_size(name = "dbh [mm]", range = c(0.5, 3), 
                 breaks = c(5, 35, 50, 100, 300, 500),  
                 labels = c(5, 35, 50, 100, 300, 500), limits = c(0, NA)) +
      theme_classic() +
      theme(legend.position = "right",
            legend.box = "vertical",
            legend.title = element_text(face = "bold"),
            panel.border = element_rect(colour = "black", fill = NA, size = 1),
            legend.key.size = unit(1.5, 'lines')) +
      ggtitle(title) +
      labs(x = "x [m]", y = "y [m]") +
      coord_equal() +
      theme(plot.margin = unit(c(1, 5, 1, 1), "lines"))
  }

  # Generate plots
  ggp_Reference <- create_plot(ppp_df_reference, "Reference point pattern")
  ggp_Reconstructed <- create_plot(ppp_df_reconstructed, "Reconstructed point pattern")

  # Combine plots using patchwork
  patchwork <- (ggp_Reference + theme(legend.position = "none") | 
                ggp_Reconstructed) + 
    plot_annotation(title = "Multi-trait point pattern reconstruction", 
                    subtitle = "Reconstruction of two marks (dbh and tree species)")

  message("Look under Plots to see the result.")
  if (nchar(names(reconstruction[1])) != 9) {
    message("As multiple reconstructions were performed, the pattern from the first reconstruction is shown as the reconstructed point pattern in the figure.")
  }
  return(patchwork)
}
