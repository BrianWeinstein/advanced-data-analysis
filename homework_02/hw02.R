
################################################################################
# Brian Weinstein - bmw2148
# STAT S4201 001
# Homework 2
# 2016-02-10

# set working directory
setwd("~/Documents/advanced-data-analysis/homework_02")

# load packages
library(dplyr)
library(Sleuth3) # Data sets from Ramsey and Schafer's "Statistical Sleuth (3rd ed)"
# library(ggplot2); theme_set(theme_bw())



# Problem 1 #######################################################################

# Define a fn to calcuate the conf interval for the sample var, given the sample var, n, and conf level
MakeVarConfidenceInterval <- function(sampleVar, n, confLevel){
  
  sigLevel <- 1 - confLevel
  
  lowerBound <- (n-1) * sampleVar / qchisq(p=(1-(sigLevel/2)), df=(n-1), lower.tail=TRUE)
  upperBound <- (n-1) * sampleVar / qchisq(p=(sigLevel/2), df=(n-1), lower.tail=TRUE)
  
  confidenceInterval <- c(lowerBound, upperBound)
  
  return(confidenceInterval)
  
}

# define variables for this problem
variance <- 1
mean <- 0
sample_size <- 10

# Initialize a counter at 0
numTrialsCapturingTrueVar=0

# Run 1000 trials, checking if the 95% CI captures the true varaince
set.seed(1)
for(trial in 1:1000){
  
  # Generate a random sample from N(mu=0, var=1)
  random_sample <- rnorm(n=sample_size, mean=mean, sd=sqrt(variance))
  
  # Compute the sample var of the random sample
  sample_var <- var(random_sample)
  
  # Compute a 95% CI for the sample variance
  confInt <- MakeVarConfidenceInterval(sampleVar=sample_var, n=sample_size, confLevel=0.95)
  
  # Check if CI captured true sample variance and increment counter
  numTrialsCapturingTrueVar = numTrialsCapturingTrueVar + ifelse(confInt[1] < variance & confInt[2] > variance, 1, 0)

}

c(numTrialsCapturingTrueVar, trial)
numTrialsCapturingTrueVar/trial

rm(list = ls()) # clear working environment



# Problem 2: Ramsey 3.22 #######################################################################

# Data input
time26 <- c(5.79, 1579.52, 2323.70)
time28 <- c(68.8, 108.29, 110.29, 426.07, 1067.60)

# Part a ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

Y1 <- log(time26) ; Y1
Y2 <- log(time28) ; Y2

# Part b ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

mean(Y1)
mean(Y2)

mean(Y1) - mean(Y2)

# Part c ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

exp(mean(Y1) - mean(Y2))

# Part d ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# Define variables to calculate confidence intervals
logDiff <- mean(Y1) - mean(Y2)
pooledSD <- sqrt( ((length(Y1) - 1)*var(Y1) + (length(Y2) - 1)*var(Y2)) / (length(Y1) + length(Y2) -2) )
se <- pooledSD * sqrt((1/3) + (1/5))
quantile <- qt(1-(0.05/2), df=(3+5-2))

# Compute confidence interval
logDiff - (quantile * se)
logDiff + (quantile * se)

# Compute anilogarithm of confidence interval
exp(logDiff - (quantile * se))
exp(logDiff + (quantile * se))

rm(list = ls()) # clear working environment



# Problem 3: Ramsey 3.25 #######################################################################

# load data
agentOrangeData <- Sleuth3::case0302

# compute group difference, 1-sided p-value, and 95% CI with all observations
t1 <- t.test(formula=Dioxin~Veteran, data=agentOrangeData,
       var.equal=TRUE, conf.level=0.95, alternative="less")
-diff(t1$estimate)[[1]]
t1$p.value
t.test(formula=Dioxin~Veteran, data=agentOrangeData,
       var.equal=TRUE, conf.level=0.95)$conf.int

# compute group difference, 1-sided p-value, and 95% CI without observation 646
t2 <- t.test(formula=Dioxin~Veteran, data=agentOrangeData[-646, ],
       var.equal=TRUE, conf.level=0.95, alternative="less")
-diff(t2$estimate)[[1]]
t2$p.value
t.test(formula=Dioxin~Veteran, data=agentOrangeData,
       var.equal=TRUE, conf.level=0.95)$conf.int

# compute group difference, 1-sided p-value, and 95% CI without observations 646 and 645
t3 <- t.test(formula=Dioxin~Veteran, data=agentOrangeData[-(645:646), ],
       var.equal=TRUE, conf.level=0.95, alternative="less")
-diff(t3$estimate)[[1]]
t3$p.value
t.test(formula=Dioxin~Veteran, data=agentOrangeData,
       var.equal=TRUE, conf.level=0.95)$conf.int

rm(list = ls()) # clear working environment



# Problem 4: Ramsey 3.28 #######################################################################



rm(list = ls()) # clear working environment



# Problem 5: Ramsey 3.32 #######################################################################



rm(list = ls()) # clear working environment



# Problem 6: Ramsey 4.19 #######################################################################



rm(list = ls()) # clear working environment


