
################################################################################
# Brian Weinstein - bmw2148
# STAT W4201 001
# Homework 7
# 2016-03-28

# set working directory
setwd("~/Documents/advanced-data-analysis/homework_07")

# prevent R from printing large numbers in scientific notation
options(scipen=5)

# load packages
library(Sleuth3) # Data sets from Ramsey and Schafer's "Statistical Sleuth (3rd ed)"
library(ggplot2); theme_set(theme_bw())
library(GGally)
library(ggrepel)
library(dplyr)
library(MASS)
# library(tidyr)
# library(formula.tools)
# library(gridExtra)



# Problem 1: Ramsey 10.21  #######################################################################

# no code needed



# Problem 2: Ramsey 10.22  #######################################################################

# no code needed



# Problem 3: Ramsey 10.26  #######################################################################

# load data
ozoneData <- Sleuth3::ex1026

# convert Surface to an indicator variable (lets us force the lm intercept to 0)
ozoneData <- ozoneData %>%
  mutate(Surface = as.integer(ifelse(Surface=="Surface", 1, 0)))

# plot of Inhibit vs UVB
ggplot(ozoneData, aes(x=UVB, y=Inhibit,
                      color=factor(Surface), shape=factor(Surface))) +
  geom_point(size=2)
ggsave(filename="writeup/3_eda.png", width=6.125, height=3.5, units="in")

# fit a linear model
lmFull <- lm(Inhibit ~ Surface*UVB, data=ozoneData)
summary(lmFull)$coefficients
confint(lmFull, level = 0.95)

# fit a linear model that forces the intercept to 0
lmZeroInt <- lm(Inhibit ~ 0 + Surface*UVB, data=ozoneData)
summary(lmZeroInt)$coefficients

rm(list = ls()) # clear working environment



# Problem 4: Ramsey 11.8  #######################################################################

# Part a ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# no code needed

# Part b ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# create a dataset with a high-leverage, low-influence obs
set.seed(1)
data_b <- data.frame(x=runif(n = 14, min = 0, max = 5)) %>%
  mutate(y= x + rnorm(n = 14, mean = 0, sd = 0.25))
data_b <- rbind(data_b, data.frame(x=7.5, y=7.55))

# plot
ggplot(data_b, aes(x=x, y=y)) + geom_point()
ggsave(filename="writeup/4b.png", width=6.125, height=3.5, units="in")

# Part c ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# create a dataset with a high-leverage, high-influence obs
set.seed(1)
data_c <- data.frame(x=runif(n = 14, min = 0, max = 5)) %>%
  mutate(y= x + rnorm(n = 14, mean = 0, sd = 0.25))
data_c <- rbind(data_c, data.frame(x=7.5, y=1.8))

# plot
ggplot(data_c, aes(x=x, y=y)) + geom_point()
ggsave(filename="writeup/4c.png", width=6.125, height=3.5, units="in")

rm(list = ls()) # clear working environment



# Problem 5: Ramsey 11.16  #######################################################################

# load data
fpmData <- Sleuth3::case1101

# fit a linear model of Metabol on Gastric*Sex
lmFpm <- lm(formula = Metabol ~ Gastric*Sex, data = fpmData)
summary(lmFpm)$coefficients

# plot of Metabol vs Gastric
ggplot(fpmData, aes(x=Gastric, y=Metabol, color=Sex, shape=Sex, label = Subject)) +
  geom_point(size=2) +
  geom_text(data=filter(fpmData, Subject==32), nudge_x = 0.10, hjust = 0, show.legend = FALSE) +
  theme(legend.position = c(0.85, 0.2))
ggsave(filename="writeup/5.png", width=6.125, height=3.5, units="in")

# calculate leverage for case 32
hatvalues(lmFpm)[32]

# calculate the studentized residual for case 32
studres(lmFpm)[32]

# calculate Cook's Distance for case 32
cooks.distance(lmFpm)[32]




rm(list = ls()) # clear working environment



# Problem 6: Ramsey 11.20  #######################################################################



rm(list = ls()) # clear working environment
