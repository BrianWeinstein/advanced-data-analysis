
################################################################################
# Brian Weinstein - bmw2148
# STAT W4201 001
# Homework 3
# 2016-02-17

# set working directory
setwd("~/Documents/advanced-data-analysis/homework_03")

# load packages
library(dplyr)
library(Sleuth3) # Data sets from Ramsey and Schafer's "Statistical Sleuth (3rd ed)"
library(ggplot2); theme_set(theme_bw())



# Problem 1: Ramsey 4.30  #######################################################################



rm(list = ls()) # clear working environment



# Problem 2: Ramsey 4.32 #######################################################################



rm(list = ls()) # clear working environment



# Problem 3: Ramsey 5.19 #######################################################################

# input data
cavityData <- data.frame(n=c(127, 44, 24, 41, 18, 16, 11, 7, 6),
                         logMean=c(7.347, 7.368, 7.418, 7.487, 7.563, 7.568, 8.214, 8.272, 8.297),
                         logSampleSd=c(0.4979, 0.4235, 0.3955, 0.3183, 0.3111, 0.4649, 0.2963, 0.3242, 0.5842))

# Part a ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# pooled estimate of variance
pooledVar <- cavityData %>%
  mutate(numeratorTerm=((n-1)*logSampleSd^2),
         denominatorTerm=(n-1)) %>%
  summarize(numerator=sum(numeratorTerm),
            denomintor=sum(denominatorTerm))
pooledVar <- pooledVar$numerator / pooledVar$denomintor
pooledVar

# Part b ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# sum of squares within
ssw <- cavityData %>%
  mutate(term=((n-1)*logSampleSd)) %>%
  summarize(sumOfSquaresWithin=sum(term))
ssw


# total sum of squares
sst <- (sum(cavityData$n) - 1) * 0.4962
sst


# sum of squares between
sst - ssw

# p-value of F-statistic
pf(q=6.5300, df1=8, df2=285, lower.tail=FALSE)



# Part c ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# compute the overall mean using a weight average of the group means
overallLogMean <- sum(cavityData$n * cavityData$logMean) / sum(cavityData$n)
overallLogMean

# recomopute the sum of squares between
sum((cavityData$n * (cavityData$logMean)^2 )) - (294 * overallLogMean^2)


rm(list = ls()) # clear working environment



# Problem 4 #######################################################################



rm(list = ls()) # clear working environment



# Problem 5 #######################################################################



rm(list = ls()) # clear working environment



# Problem 6 #######################################################################



rm(list = ls()) # clear working environment



# Problem 7 #######################################################################



rm(list = ls()) # clear working environment

