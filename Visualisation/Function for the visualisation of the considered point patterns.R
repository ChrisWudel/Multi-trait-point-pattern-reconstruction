################################################################################
#       Function for the visualisation of the considered point patterns        #
################################################################################
vis_pp <- function(reconstruction) {
  
#install.packages("ggplot2")
#install.packages("patchwork")
library(ggplot2)
library(patchwork)# root neck diameter (rnd)

################################################################################## Data import from the results of point pattern reconstruction.
ppp_reference     <- reconstruction$reference
ppp_reconstructed <- reconstruction$reconstructed

################################################################################## Visualisation of the reference point pattern.                           
Faktor <- 1000
ppp_dataframe <- data.frame(cbind (ppp_reference$x, ppp_reference$y, 
                                   size = ppp_reference$marks[2], 
                                   ppp_reference$marks[1] * Faktor))
colnames(ppp_dataframe) <- c("x", "y", "species", "dbh")

stock_layer <- 1
for ( i in 1:nrow(ppp_dataframe))         
{
  if(ppp_dataframe$dbh[i] > 100)
  {
    stock_layer[i] <- "Upper stand"
  }
  if(ppp_dataframe$dbh[i] <= 100)
  {
    stock_layer[i] <- "Rejuvenation"
  }
}

ppp_dataframe <- cbind(ppp_dataframe, stock_layer)
colnames(ppp_dataframe) <- c("x", "y", "species", "dbh", "stock_layer")

ggp_Reference <-
  ggplot(data = ppp_dataframe, aes(x, y, size = dbh)) +
  geom_point(aes(size = dbh, shape = stock_layer, color = species),) +
  scale_shape_manual(name   = "stock layer", 
                     labels = c("Rejuvenation", "Upper stand"), 
                     values = c(13,16)) +
  scale_size(name = "dbh [mm]", range = c(0.5, 5), 
             breaks = c(5, 35, 50, 100, 300, 500),  
             labels = c(5, 35, 50, 100, 300, 500), limits = c(0, NA))+
  xlim(ppp_reconstructed$window$xrange[1], ppp_reconstructed$window$xrange[2]) +
  ylim(ppp_reconstructed$window$yrange[1], ppp_reconstructed$window$yrange[2]) +
  theme_classic() +
  theme(legend.position = "right",
        legend.box      = "vertical",
        legend.title    = element_text(face = "bold"),
        panel.border    = element_rect(colour = "black", fill = NA, size = 1),
        legend.key.size = unit(1.5, 'lines')) +
  ggtitle("Reference point pattern") +
  labs(x = "x [m]", y = "y [m]") +
  coord_equal()+
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"))

################################################################################## Visualisation of the reconstructed point pattern.
ppp_dataframe <- data.frame(cbind (ppp_reconstructed$x, ppp_reconstructed$y, 
                                   size = ppp_reconstructed$marks[2], 
                                   ppp_reconstructed$marks[1] * Faktor))
colnames(ppp_dataframe) <- c("x", "y", "species", "dbh")

stock_layer <- 1
for ( i in 1:nrow(ppp_dataframe))         
{
  if(ppp_dataframe$dbh[i] > 100)
  {
    stock_layer[i] <- "Upper stand"
  }
  if(ppp_dataframe$dbh[i] <= 100)
  {
    stock_layer[i] <- "Rejuvenation"
  }
}

ppp_dataframe <- cbind(ppp_dataframe, stock_layer)
colnames(ppp_dataframe) <- c("x", "y", "species", "dbh", "stock_layer")

ggp_Reconstructed <-
  ggplot(data = ppp_dataframe, aes(x, y, size = dbh)) +
  geom_point(aes(size = dbh, shape = stock_layer, color = species),) +
  scale_shape_manual(name   = "stock layer", 
                     labels = c("Rejuvenation", "Upper stand"), 
                     values = c(13,16)) +
  scale_size(name = "dbh [mm]", range = c(0.5, 5), 
             breaks = c(5, 35, 50, 100, 300, 500),  
             labels = c(5, 35, 50, 100, 300, 500), limits = c(0, NA))+
  xlim(ppp_reconstructed$window$xrange[1], ppp_reconstructed$window$xrange[2]) +
  ylim(ppp_reconstructed$window$yrange[1], ppp_reconstructed$window$yrange[2]) +
  theme_classic() +
  theme(legend.position = "right",
        legend.box      = "vertical",
        legend.title    = element_text(face = "bold"),
        panel.border    = element_rect(colour = "black", fill = NA, size = 1),
        legend.key.size = unit(1.5, 'lines')) +
  ggtitle("Reconstructed point pattern") +
  labs(x = "x [m]", y = "y [m]") +
  coord_equal() +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"))

################################################################################## Comparison of the dot patterns
patchwork <- (ggp_Reference + theme(legend.position = "none") | 
              ggp_Reconstructed)+ plot_annotation(
                title = "Point pattern reconstruction", 
                subtitle = "Reconstruction of two marks (dbh and tree species)")

print("look under Plots to see the result.")
return(patchwork)

}
