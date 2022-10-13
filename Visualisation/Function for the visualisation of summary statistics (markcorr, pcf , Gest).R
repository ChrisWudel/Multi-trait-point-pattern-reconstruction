################################################################################
#        Function for plotting the Mark Correlation Function (markcorr), the   #     
#                     Pair Correlation Function (pcf) and                      #
#               the Nearest Neighbour Distance Function G (Gest)               #
################################################################################
plot_sum_stat <- function(reconstruction) {
  
#install.packages("ggplot2")
#install.packages("reshape")
library(ggplot2)
library(reshape)

################################################################################## Data import from the results of point pattern reconstruction.
ppp_reference     <- reconstruction$reference
ppp_reconstructed <- reconstruction$reconstructed
rmin              <- 0
rmax              <- as.numeric(reconstruction$Parameter_setting$Value[5])
rcount            <- as.numeric(reconstruction$Parameter_setting$Value[4])
r                 <- seq(rmin, rmax, length.out = rcount)
bw                <- as.numeric(reconstruction$Parameter_setting$Value[12])

################################################################################## Visualising the mark correlation function (markcorr) of the results of the point pattern reconstruction.                     
markcorr               <- markcorr(ppp_reference, correction = "Ripley", r = r)
markcorr_recon         <- markcorr(ppp_reconstructed, correction = "Ripley", 
                                   r = r)
dbh_markcorr           <- markcorr$diameter
species_markcorr       <- markcorr$species
dbh_markcorr_recon     <- markcorr_recon $diameter
species_markcorr_recon <- markcorr_recon $species

markcorr_all <- data.frame(cbind(dbh_markcorr$iso, species_markcorr$iso, 
                                 dbh_markcorr_recon$iso, 
                                 species_markcorr_recon$iso, 
                                 markcorr$diameter$r))
colnames(markcorr_all) <- c("Reference mark dbh","Reconstructed mark dbh",
                            "Reference mark species",
                            "Reconstructed mark species","r")

df1 <- na.omit(data.frame(melt(markcorr_all,"r")))

ggp_markcorr <-
ggplot(data = df1, aes(x = r, y = value, col = variable, linetype = variable)) +
  geom_line() +
  theme_bw() +
  scale_color_manual(values    = c("blue", "red", "royalblue4", "orangered4")) +
  scale_linetype_manual(values = c(1, 1, 2, 2)) +
  ggtitle("markcorr") +
  labs(y = "kmm(r)",x = "r [m]") +
  theme(legend.title = element_blank())

################################################################################## Visualising the Pair Correlation Function (pcf) of the results of the point pattern reconstruction.                     
pcf       <- spatstat.core::pcf.ppp(X = ppp_reference, bw = bw, r = r, 
                                    kernel = "gaussian", stoyan = 0, 
                                    correction = "none", divisor = "d")
pcf_recon <- spatstat.core::pcf.ppp(X = ppp_reconstructed,bw = bw, r = r, 
                                    kernel = "gaussian", stoyan = 0,
                                    correction = "none", divisor = "d")

pcf_all           <- data.frame(cbind(pcf$un, pcf_recon$un, pcf$theo,pcf$r))
colnames(pcf_all) <- c("Reference", "Reconstruction", "Theoretical","r")

df2               <- na.omit(data.frame(melt(pcf_all,"r")))

ggp_pcf <-
ggplot(data = df2, aes(x = r, y = value, col = variable, linetype = variable)) +
  geom_line() +
  theme_bw() +
  scale_color_manual(values = c("blue", "red", "black")) +
  scale_linetype_manual(values = c(1, 2, 2)) +
  ggtitle("pcf") +
  labs(y = "g(r)", x = "r [m]") +
  theme(legend.title = element_blank())
  
################################################################################## Visualising the Nearest Neighbour Distance Function G (Gest) of the results of the point pattern reconstruction.                     
gest       <- spatstat.core::Gest(X = ppp_reference, correction = "han", r = r)
gest_recon <- spatstat.core::Gest(X = ppp_reconstructed, correction = "han", 
                                  r = r)

gest_all           <- data.frame(cbind(gest$han, gest_recon$han, gest$theo,gest$r))
colnames(gest_all) <- c("Reference", "Reconstruction", "Theoretical","r")

df3                <- na.omit(data.frame(melt(gest_all,"r")))

ggp_gest <-
ggplot(data = df3, aes(x = r, y = value, col = variable, linetype = variable)) +
  geom_line() +
  theme_bw() +
  scale_color_manual(values = c("blue", "red", "black")) +
  scale_linetype_manual(values = c(1, 2, 2)) +
  ggtitle("gest") +
  labs(y = "G(r)", x = "r [m]") +
  theme(legend.title = element_blank())

print("look under Plots to see the result.")
result <- list(ggp_markcorr, ggp_pcf, ggp_gest)

return(result)

}
