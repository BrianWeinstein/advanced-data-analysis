
################################################################################
# Brian Weinstein - bmw2148
# STAT W4201 001
# Homework 6
# 2016-03-09

# set working directory
setwd("~/Documents/advanced-data-analysis/homework_06")

# prevent R from printing large numbers in scientific notation
# options(scipen=5)

# load packages
library(Sleuth3) # Data sets from Ramsey and Schafer's "Statistical Sleuth (3rd ed)"
library(ggplot2); theme_set(theme_bw())
# library(gridExtra)
library(GGally)
# library(dplyr)



# Problem 1: Ramsey 9.14  #######################################################################

# load data
paceData <- Sleuth3::ex0914

# Part a ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# plot a matrix of pairwise scatterplots
plot.pairs <- ggpairs(data=paceData,
                      lower=list(continuous=wrap("points", size=0.7)))
plot.pairs
png(filename="writeup/1a.png", width=11, height=9, units="in", res=300)
print(plot.pairs)
dev.off()

# Part b ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# least squares fit of lin reg of Heart on Bank, Walk, Talk
lm1 <- lm(Heart ~ Bank + Walk + Talk, data=paceData)
summary(lm1)

# Part c ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# check the residuals of the fitted model
ggplot(lm1, aes(x=.fitted, y=.resid)) +
  geom_point() +
  geom_hline(yintercept=0, linetype="dashed") +
  labs(x="Fitted values", y="Residuals")
ggsave(filename="writeup/1c.png", width=6.125, height=3.5, units="in")

# Part d ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# get 95% CIs for the coefficients in lm1
confint(lm1, level = 0.95)

rm(list = ls()) # clear working environment



# Problem 2: Ramsey 9.16  #######################################################################

# Part a ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###



rm(list = ls()) # clear working environment



# Problem 3: Ramsey 9.18  #######################################################################


rm(list = ls()) # clear working environment



# Problem 4: Ramsey 9.20  #######################################################################


rm(list = ls()) # clear working environment



# Problem 5: Ramsey 10.19  #######################################################################


rm(list = ls()) # clear working environment



# Problem 6: Ramsey 10.28  #######################################################################


rm(list = ls()) # clear working environment



