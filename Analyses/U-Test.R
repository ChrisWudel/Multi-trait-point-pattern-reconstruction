################################################################################
#     U-tests to check whether there are significant deviations between the    #
#        calibration variants, values in the table represent the p-value.      #
################################################################################## Import of the energy values of the calibration from Github.
source("https://raw.githubusercontent.com/ChrisWudel/Point-pattern-reconstruction/main/Records/Results%20(energies)%20of%20the%20simulation%20experiments%20to%20calibrate.csv")

################################################################################## Definition of an auxiliary matrix and the count variables. 
result <- matrix(data = NA, nrow = 9, ncol = 9)
i  <- 1
t  <- 1
tt <- 1
R  <- 2
RR <- 2

################################################################################## Loop (while) to perform the U-tests for the respective calibration variants. 
while(i < 82) {
  test_result   <- wilcox.test(data[, R], data[, RR], exact = FALSE)
  result[tt, t] <- round(test_result$p.value, 4)
  t             <- t + 1
  RR            <- RR + 1
  
  if(i / 9 == i %/% 9 ){
    R  <- R + 1
    t  <- 1
    RR <- 2
    tt <- tt + 1
  }
  i <- i + 1
}

################################################################################### Presentation of the results in table form.
names <- c("Value of authorship", "Equal distribution", 
     "Mean values of the uniform distribution", "Extreme value p1", 
     "Extreme value p2", "Extreme value p3", "Extreme value p4", 
     "Extreme value p5", "Extreme value p6")

result <- data.frame(matrix(names), result)
colnames(result)<- c("",names) 
