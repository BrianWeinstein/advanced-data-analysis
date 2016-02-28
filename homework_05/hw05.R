
################################################################################
# Brian Weinstein - bmw2148
# STAT W4201 001
# Homework 5
# 2016-03-02

# set working directory
setwd("~/Documents/advanced-data-analysis/homework_05")

# prevent R from printing large numbers in scientific notation
options(scipen=5)

# load packages
library(Sleuth3) # Data sets from Ramsey and Schafer's "Statistical Sleuth (3rd ed)"
library(ggplot2); theme_set(theme_bw())
library(dplyr)
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

# Part a ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# calculate the standard error of the prediction
sep <- sigmaHat * sqrt(1 + (1/n) + ((x0 - xBar)^2 /  ((n-1) * sampleVarX))) ; sep

# Part b ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# predicted value at x0=log(5)
pred <- beta0 + (beta1 * x0) ; pred

# 95% prediction confidence interval
t <- qt(p=0.975, df=(n-2)) ; t
lowerBound <- pred - (t * sep)
upperBound <- pred + (t * sep)
c(lowerBound, upperBound)

rm(list = ls()) # clear working environment



# Problem 2: Ramsey 7.24  #######################################################################

# load data
birthData <- Sleuth3::ex0724

# Part a ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# fit a linear model to Denmark ~ Year
lmDenmark <- lm(formula=Denmark~Year, data=birthData)
summary(lmDenmark)$coefficients

# fit a linear model to Netherlands ~ Year
lmNetherlands <- lm(formula=Netherlands~Year, data=birthData)
summary(lmNetherlands)$coefficients

# fit a linear model to Canada ~ Year
lmCanada <- lm(formula=Canada~Year, data=birthData)
summary(lmCanada)$coefficients

# fit a linear model to USA ~ Year
lmUsa <- lm(formula=USA~Year, data=birthData)
summary(lmUsa)$coefficients

# Part b ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# Denmark
summary(lmDenmark)$coefficients
c(-2.072598, 4.423828e-02/2)

# Netherlands
summary(lmNetherlands)$coefficients
c(-5.710196, 9.636921e-07/2)

# Canada
summary(lmCanada)$coefficients
c(-4.016653, 7.375947e-04/2)

# USA
summary(lmUsa)$coefficients
c(-5.779212, 1.439109e-05/2)

# Part c ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# no code needed

# Part d ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# calculate the RSS for the USA model
sum((lmUsa$residuals)^2)

# calculate the RSS for the Canada model
sum((lmCanada$residuals)^2)

# Part e ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# no code needed

rm(list = ls()) # clear working environment



# Problem 3: Ramsey 7.28  #######################################################################

# load data
brainData <- Sleuth3::ex0728

# Part a ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# create a variable groupin the subjects into "control" and "player" groups
brainData <- brainData %>%
  mutate(Group=as.factor(ifelse(Years==0, "control", "player")))

# boxplots of brain activity for each group
plot.orig <- ggplot(brainData, aes(x=Group, y=Activity)) +
  # geom_violin(alpha=0.15) +
  geom_boxplot() +
  labs(y="Neuronal activity index", x="Group")
plot.orig
ggsave(filename="writeup/3_orig.png", width=6.125, height=3.5, units="in")

# create a log(Activity) variable
brainData <- brainData %>%
  mutate(LogActivity=log(Activity))

# boxplots of brain activity for each group
plot.log <- ggplot(brainData, aes(x=Group, y=LogActivity)) +
  # geom_violin(alpha=0.15) +
  geom_boxplot() +
  labs(y="Neuronal activity index (log scale)", x="Group")
plot.log
ggsave(filename="writeup/3_log.png", width=6.125, height=3.5, units="in")

# compare group standard deviations on the original and log scales
brainData %>%
  group_by(Group) %>%
  summarize(sd(Activity), sd(LogActivity)) %>%
  as.data.frame()

# Perform a two-sample t-test
tt <- t.test(formula=LogActivity~Group, data=brainData,
             var.equal=TRUE, conf.level=0.95, alternative="two.sided")
tt

# take antilog of the estimate
exp(diff(tt$estimate)[[1]])

# Perform a two sided t-test for the confidence interval and take antilog
exp(-tt$conf.int)

# Part b ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

ggplot(brainData, aes(x=Years, y=Activity)) +
  geom_point() +
  geom_smooth(method=lm)
















rm(list = ls()) # clear working environment



# Problem 4: Ramsey 8.17  #######################################################################



rm(list = ls()) # clear working environment



# Problem 5: Ramsey 8.20  #######################################################################



rm(list = ls()) # clear working environment



# Problem 6: Ramsey 9.12  #######################################################################



rm(list = ls()) # clear working environment