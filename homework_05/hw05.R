
################################################################################
# Brian Weinstein - bmw2148
# STAT W4201 001
# Homework 5
# 2016-03-02

# set working directory
setwd("~/Documents/advanced-data-analysis/homework_05")

# load packages
library(Sleuth3) # Data sets from Ramsey and Schafer's "Statistical Sleuth (3rd ed)"
# library(ggplot2); theme_set(theme_bw())
# library(dplyr)
# library(scales)
# library(gmodels)
# library(agricolae)



# Problem 1: Ramsey 7.18  #######################################################################

# define coefficients and constants from Display 7.12
beta0 <- 6.9836
beta1 <- -0.7257
sigmaHat <- 0.08226
n <- 10
xBar <- 1.190
sampleVarX <- 0.6344
x0 <- log(5)

# calculate the standard error of the prediction
sep <- sigmaHat * sqrt(1 + (1/n) + ((x0 - xBar)^2 /  ((n-1) * sampleVarX))) ; sep

# predicted value at x0=log(5)
pred <- beta0 + (beta1 * x0) ; pred

# 95% prediction confidence interval
t <- qt(p=0.975, df=(n-2)) ; t
lowerBound <- pred - (t * sep)
upperBound <- pred + (t * sep)
c(lowerBound, upperBound)

rm(list = ls()) # clear working environment



# Problem 2: Ramsey 7.24  #######################################################################



rm(list = ls()) # clear working environment



# Problem 3: Ramsey 7.28  #######################################################################



rm(list = ls()) # clear working environment



# Problem 4: Ramsey 8.17  #######################################################################



rm(list = ls()) # clear working environment



# Problem 5: Ramsey 8.20  #######################################################################



rm(list = ls()) # clear working environment



# Problem 6: Ramsey 9.12  #######################################################################



rm(list = ls()) # clear working environment