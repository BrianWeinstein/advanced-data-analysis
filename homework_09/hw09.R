
################################################################################
# Brian Weinstein - bmw2148
# STAT W4201 001
# Homework 9
# 2016-04-13

# set working directory
setwd("~/Documents/advanced-data-analysis/homework_09")

# prevent R from printing large numbers in scientific notation
options(scipen=5)

# load packages
library(Sleuth3) # Data sets from Ramsey and Schafer's "Statistical Sleuth (3rd ed)"
library(ggplot2); theme_set(theme_bw())
# library(MASS)
library(dplyr)
# library(GGally)
# library(leaps)
# library(tidyr)
# library(ggrepel)
# library(gridExtra)



# Problem 1: Ramsey 20.12 #######################################################################

# load data
mdData <- Sleuth3::ex2012

# Part a ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# scatterplot
ggplot(mdData, aes(x=log(CK), y=H)) +
  geom_point(aes(color=Group, shape=Group), size=2)
ggsave(filename="writeup/1a.png", width=6.125, height=3.5, units="in")

# Part b ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# fit a logistic regression model on CK and CK^2
glm_1b1 <- glm(formula = Group ~ CK + I(CK^2),
               data = mdData, family = binomial)
summary(glm_1b1)$coefficients

# fit a logistic regression model on log(CK) and log(CK)^2
glm_1b2 <- glm(formula = Group ~ log(CK) + I(log(CK)^2),
               data = mdData, family = binomial)
summary(glm_1b2)$coefficients

# scatterplot
ggplot(mdData, aes(x=CK, y=H)) +
  geom_point(aes(color=Group, shape=Group), size=2)
ggsave(filename="writeup/1b.png", width=6.125, height=3.5, units="in")

# Part c ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# fit a logistic regression model on log(CK) and H
glm_1c <- glm(formula = Group ~ log(CK) + H,
               data = mdData, family = binomial)
summary(glm_1c)$coefficients

# Part d ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# fit a reduced model
glm_1d <- glm(formula = Group ~ 1,
              data = mdData, family = binomial)
summary(glm_1d)$coefficients

# perform the likelihood ratio test (drop-in-deviance test)
anova(glm_1c, glm_1d, test="LRT")

# Part e ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# calculate odds at CK=80, H=85
exp(predict(glm_1c, data.frame(CK=80, H=85)))

# calculate odds at CK=300, H=100
1/(1 +exp(-predict(glm_1c, data.frame(CK=300, H=100))))
1/(1 +exp(-predict(glm_1c, data.frame(CK=80, H=85))))


rm(list = ls()) # clear working environment



# Problem 2: Ramsey 21.16 #######################################################################



rm(list = ls()) # clear working environment



# Problem 3 #######################################################################

# no code needed
