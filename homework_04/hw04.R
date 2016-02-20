
################################################################################
# Brian Weinstein - bmw2148
# STAT W4201 001
# Homework 4
# 2016-02-24

# set working directory
setwd("~/Documents/advanced-data-analysis/homework_04")

# load packages
library(dplyr)
library(Sleuth3) # Data sets from Ramsey and Schafer's "Statistical Sleuth (3rd ed)"
library(ggplot2); theme_set(theme_bw())
# library(data.table)



# Problem 1: Ramsey 5.23  #######################################################################

# load data
trexData <- Sleuth3::ex0523 %>%
  mutate(BoneNumber=as.factor(as.integer(gsub("Bone", "", Bone))))

# boxplots of oxygen composition in each bone
ggplot(trexData, aes(x=BoneNumber, y=Oxygen)) +
  geom_violin(alpha=0.15) +
  geom_boxplot() +
  geom_point(alpha=0.15)+
  labs(y="Oxygen Isotopic Composition\n(per mil deviations from SMOW)", x="Bone Number")
ggsave(filename="writeup/1.png", width=6.125, height=3.5, units="in")

# use a one way ANOVA F-test
anovaTable <- anova(lm(Oxygen~Bone, data=trexData)); anovaTable

# compute the total sum of squares and the total degrees of freedom
sum(anovaTable$'Sum Sq')
sum(anovaTable$Df)

rm(list = ls()) # clear working environment



# Problem 2: Ramsey 5.25  #######################################################################



rm(list = ls()) # clear working environment


# Problem 3: Ramsey 6.12  #######################################################################



rm(list = ls()) # clear working environment



# Problem 4: Ramsey 6.15  #######################################################################



rm(list = ls()) # clear working environment



# Problem 5: Ramsey 6.16  #######################################################################



rm(list = ls()) # clear working environment



# Problem 6: Ramsey 6.23  #######################################################################



rm(list = ls()) # clear working environment



