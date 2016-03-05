
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
library(GGally)
library(dplyr)
library(tidyr)
# library(gridExtra)



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
lmPace <- lm(formula=Heart ~ Bank + Walk + Talk, data=paceData)
summary(lmPace)

# Part c ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# check the residuals of the fitted model
ggplot(lmPace, aes(x=.fitted, y=.resid)) +
  geom_point() +
  geom_hline(yintercept=0, linetype="dashed") +
  labs(x="Fitted values", y="Residuals")
ggsave(filename="writeup/1c.png", width=6.125, height=3.5, units="in")

# Part d ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# get 95% CIs for the coefficients in lmPace
confint(lmPace, level = 0.95)

rm(list = ls()) # clear working environment



# Problem 2: Ramsey 9.16  #######################################################################

# load data
pollenData <- Sleuth3::ex0327

# Part a ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# scatterplot of pollen vs duration, by bee type
ggplot(pollenData, aes(x=DurationOfVisit, y=PollenRemoved, color=BeeType, shape=BeeType)) +
  geom_point(size=2.5)
ggsave(filename="writeup/2a.png", width=6.125, height=3.5, units="in")

# Part b ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# create a logit-transformed "proportion of pollen" variable
pollenData <- pollenData %>%
  mutate(LogitPollenRemoved=log(PollenRemoved/(1-PollenRemoved)))

# scatterplot of proportion of pollen pollen vs duration, by bee type
ggplot(pollenData, aes(x=DurationOfVisit, y=LogitPollenRemoved, color=BeeType, shape=BeeType)) +
  geom_point(size=2.5)
ggsave(filename="writeup/2b.png", width=6.125, height=3.5, units="in")

# Part c ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# create a log-transformed duration variable
pollenData <- pollenData %>%
  mutate(LogDurationOfVisit=log(DurationOfVisit))

# scatterplot of proportion of pollen pollen vs duration, by bee type
ggplot(pollenData, aes(x=LogDurationOfVisit, y=LogitPollenRemoved, color=BeeType, shape=BeeType)) +
  geom_point(size=2.5)
ggsave(filename="writeup/2c.png", width=6.125, height=3.5, units="in")

# Part d ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# create a linear regression model of LogitPollenRemoved on LogDurationOfVisit * BeeType
lmPollen <- lm(formula=LogitPollenRemoved ~ LogDurationOfVisit * BeeType,
               data=pollenData)
summary(lmPollen)$coefficients

# Part e ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# create a linear regression model of LogitPollenRemoved on LogDurationOfVisit + BeeType
lmPollenNoInt <- lm(formula=LogitPollenRemoved ~ LogDurationOfVisit + BeeType,
               data=pollenData)
summary(lmPollenNoInt)$coefficients

# get 95% CIs for the coefficients in lmPollenNoInt
confint(lmPollenNoInt, level = 0.95)

rm(list = ls()) # clear working environment



# Problem 3: Ramsey 9.18  #######################################################################

# load data
wingData <- Sleuth3::ex0918

# convert data to long format
wingDataLong <- wingData %>%
  gather(data=., key=Sex, value=Avg_WingSize, c(Females, Males)) %>%
  mutate(SE_WingSize=ifelse(Sex=="Females", SE_Females, SE_Males),
         Ratio=ifelse(Sex=="Females", Ratio, NA),
         SE_Ratio=ifelse(Sex=="Females", SE_Ratio, NA)) %>%
  select(Continent, Latitude, Sex, Avg_WingSize, SE_WingSize, Ratio, SE_Ratio) %>%
  mutate(Sex=as.factor(Sex))

# Part a ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# scatterplot of Avg_WingSize vs Latitude, by Continent and Sex
ggplot(wingDataLong, aes(x=Latitude, y=Avg_WingSize,
                         color=interaction(Continent, Sex),
                         shape=interaction(Continent, Sex))) +
  geom_point(size=2.5) +
  scale_shape_manual(values=c(16, 17, 15, 18))
ggsave(filename="writeup/3a.png", width=6.125, height=3.5, units="in")

# Part b ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# relevel the sex variable
wingDataLong$Sex <- relevel(wingDataLong$Sex, "Males")

# create a linear regression model of Avg_WingSize
# on Latitude + Sex * Continent + Latitude * Sex * Continent
lmWing <- lm(formula=Avg_WingSize ~ Latitude + Sex * Continent + Latitude * Sex * Continent,
                    data=wingDataLong)
summary(lmWing)$coefficients

rm(list = ls()) # clear working environment



# Problem 4: Ramsey 9.20  #######################################################################


rm(list = ls()) # clear working environment



# Problem 5: Ramsey 10.19  #######################################################################


rm(list = ls()) # clear working environment



# Problem 6: Ramsey 10.28  #######################################################################


rm(list = ls()) # clear working environment



