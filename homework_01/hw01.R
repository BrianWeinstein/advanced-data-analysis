
################################################################################
# Brian Weinstein - bmw2148
# STAT S4201 001
# Homework 1
# 2016-02-03

# set working directory
setwd("~/Documents/advanced-data-analysis/homework_01")

# load packages
library(dplyr)
library(Sleuth3) # Data sets from Ramsey and Schafer's "Statistical Sleuth (3rd ed)"
library(ggplot2); theme_set(theme_bw())




# Problem 1: Ramsey 1.17 ###############################################################################

# score data (a1, a2, a3, a4, b1, b2, b3)
scores <- c(68, 77, 82, 85, 53, 64, 71)

# create all combinations of 4-3 group assignments
setA <- combn(scores, 4, simplify=FALSE)
setB <- sapply(1:length(setA), function(i){list(setdiff(scores, setA[[i]]))})

# combine group assignments into dataframe
sets <- lapply(1:length(setA), function(i){c(setA[[i]], setB[[i]])}) %>%
  do.call(rbind, .) %>%
  as.data.frame()
colnames(sets) <- c("a1", "a2", "a3", "a4", "b1", "b2", "b3")

rm(setA, setB)

# calculate difference between sample averages
sets <- sets %>%
  mutate(avg_a=rowMeans(sets[, 1:4]),
         avg_b=rowMeans(sets[, 5:7]),
         avg_diff=avg_a-avg_b)

# define the observed difference in sample averages
observed_diff <- sets$avg_diff[1] # row 1 has the observed data

# caluclate two-sided p-value of observed_diff
pvalue <- sum(abs(sets$avg_diff) >= abs(observed_diff)) / nrow(sets)


rm(sets, observed_diff, pvalue, scores)




# Problem 3: Ramsey 1.25 (b) ###############################################################################

# load data
ratData <- Sleuth3::ex0125

# create boxplots
ggplot(ratData, aes(x=Group, y=Zinc)) +
  geom_boxplot() + 
  ylab("Zinc concentration (mg/ml)")
ggsave(filename="writeup/3.png", width=5, height=3, units="in")




# Problem 4 ###############################################################################

# Part a ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# calculate the observed difference in group averages
observed_avg <- ratData %>%
  group_by(Group) %>%
  summarize(groupAvg=mean(Zinc)) %>%
  arrange(Group)
observed_diff <- observed_avg$groupAvg[1] - observed_avg$groupAvg[2]

# Null hypothesis: observed_diff = 0
# Alternative hypothesis: observed_diff != 0


# Part b ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# created an index
rat.ndx <- 1:nrow(ratData)

# initialize an empty list to store the difference in group averages
avg_diff <- list()

set.seed(1)

# for 1000 group divisions
for(i in 1:1000){
  
  # select a sample of rats for group A
  ratGroupA <- sample(rat.ndx, size=20, replace=FALSE)
  ratGroupA.zinc <- ratData$Zinc[ratGroupA]
  
  # place the remaing ratis in group B
  ratGroupB <- setdiff(rat.ndx, ratGroupA)
  ratGroupB.zinc <- ratData$Zinc[ratGroupB]
  
  # calculate the differenc in group averages
  avg_diff[[i]] <- mean(ratGroupA.zinc) - mean(ratGroupB.zinc)
  
}

# vector of differences in group averages
avg_diff <- unlist(avg_diff)

# caluclate two-sided p-value of observed_diff
pvalue <- sum(abs(avg_diff) >= abs(observed_diff)) / length(avg_diff)


# Part c ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

ggplot(as.data.frame(avg_diff), aes(x=avg_diff)) +
  geom_histogram(bins=30) +
  geom_vline(xintercept=c(observed_diff, -observed_diff), linetype="dotted") +
  xlab("Test statistic t (difference between sample averages)")
ggsave(filename="writeup/4c.png", width=5, height=3, units="in")











rm(ratData, rat.ndx,
   ratGroupA, ratGroupA.zinc, ratGroupB, ratGroupB.zinc,
   i, observed_avg, observed_diff, avg_diff, pvalue)







