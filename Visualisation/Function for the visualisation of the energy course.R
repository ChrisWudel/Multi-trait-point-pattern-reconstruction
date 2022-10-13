################################################################################
#           Function for the visualisation of the energy course                #
################################################################################
vis_ener_cour <- function(reconstruction) {

#install.packages("plotly")
library(plotly)
  
################################################################################## Data import from the results of point pattern reconstruction.
x <- reconstruction$energy_total$energy_markcrosscorr$energy_markcrosscorr
y <- reconstruction$energy_total$energy_markcrosscorr_0$energy_markcrosscorr_0
z <- reconstruction$energy_total$energy_pcf$energy_pcf + 
     reconstruction$energy_total$energy_gest$energy_gest
c <- reconstruction$energy_total$energy_markcrosscorr$energy_markcrosscorr + 
     reconstruction$energy_total$energy_markcrosscorr_0$energy_markcrosscorr_0 + 
     reconstruction$energy_total$energy_pcf$energy_pcf +
     reconstruction$energy_total$energy_gest$energy_gest

################################################################################## Visualisation of the energy course.
fig <- plot_ly( x = x, y = y, z = z, type = 'scatter3d', mode = 'lines+markers', 
                marker = list(color = c, colorscale = c('#0e0e7a', '#88a302'),
                              size = 5, showscale = TRUE))


fig <- layout(fig, scene = list(xaxis = list(title = 'Correlation'),
                                   yaxis = list(title = 'Local correlation'),
                                   zaxis = list(title = 'Spatial information ')),
                      showlegend = FALSE,
                      annotations = list(
                        x         = 1.13,
                        y         = 1.05,
                        text      = ' ',
                        showarrow = FALSE
                      ))

print("look under Viewer to see the result.")
return(fig)

}
