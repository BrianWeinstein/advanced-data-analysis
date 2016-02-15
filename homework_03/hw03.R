
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
library(data.table)



# Problem 1: Ramsey 4.30  #######################################################################

# load data
spfData <- Sleuth3::ex0430

# create spfEstimate column
spfData <- spfData %>%
  mutate(spfEstimate=Sunscreen/PreTreatment)

ggplot(spfData, aes(x="", y=spfEstimate)) +
  geom_boxplot() + 
  labs(y="SPF Estimate (Sunscreen / PreTreatment)", x="")
ggsave(filename="writeup/1.png", width=6.125, height=3.5, units="in")

# Define a fn to calcuate a conf interval for a given mean, std error, df, and conf level
MakeConfidenceIntervalForMean <- function(mean, se, df, confLevel){
  
  sigLevel <- 1 - confLevel
  
  error <- qt(p=1-(sigLevel/2), df=df) * se
  
  lowerBound <- mean - error
  upperBound <- mean + error
  
  confidenceInterval <- c(lowerBound, upperBound)
  
  return(confidenceInterval)
  
}

# define variables for this problem
mean_value <- mean(spfData$spfEstimate); mean_value
std_err <- sd(spfData$spfEstimate)/sqrt(nrow(spfData)); std_err
df_value <- nrow(spfData) - 1; df_value

# 95% and 90% confidence intervals for (mu_2 - mu_1)
MakeConfidenceIntervalForMean(mean=mean_value, se=std_err, df=df_value, confLevel=0.95)

rm(list = ls()) # clear working environment



# Problem 2: Ramsey 4.32 #######################################################################

# load data and compute treatment differences
marData <- Sleuth3::ex0432 %>% mutate(diff=Marijuana-Placebo)
marData

# plot diffs
ggplot(marData, aes(x="", y=diff)) +
  geom_boxplot() + 
  labs(y="Difference in the number of retching episodes\n(Marijuana - Placebo)", x="")
ggsave(filename="writeup/2.png", width=6.125, height=3.5, units="in")

# perform sign test on hypothesis that Marijuana-Placebo < 0

numObs <- sum(marData$diff != 0) # number of nonzero observations
numPosDiffs <- sum(marData$diff > 0) # compute the number of positive differences

zStat <- (numPosDiffs - (numObs/2)) / sqrt(numObs/4) # compute the Z statistic
zStat

# compute an estimated one-sided p-value
pnorm(-1 * abs(zStat), mean=0, sd=1)

# Compute a 95% CI and find an estimate for the additive treatment effect
# test several hypothesized values for the treatment effect
# and find the smallest and largest deltas that lead to a
# two-sided pvalue >= 0.05. Those are the endpoints of a
# 95% CI, with the midpoint as the estimate for delta.

# initialize an empty list
deltaPvalList <- list()

# get 2 sided pvalues for various hypothetical deltas
for(i in 1:50){
  
  delta <- i
  marDataNew <- marData %>%
    mutate(MarijuanaNew=Marijuana+delta,
           diffNew=MarijuanaNew-Placebo)
  
  numObs <- sum(marDataNew$diffNew != 0)
  numPosDiffs <- sum(marDataNew$diffNew > 0)
  zStat <- (numPosDiffs - (numObs/2)) / sqrt(numObs/4)
  pval <- 2 * pnorm(q=(-1 * abs(zStat)), mean=0, sd=1) # 2-sided p-value
  
  deltaPvalList[[i]] <- data.frame(delta=delta, pval=pval)
  
}

deltaPvalList <- rbindlist(deltaPvalList) %>% as.data.frame()

# find the min and max deltas that lead to a two-sided p-value >= 0.05
confInt <- deltaPvalList %>% filter(pval >= 0.05) %>% select(delta) %>% range()
confInt
mean(confInt)

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

