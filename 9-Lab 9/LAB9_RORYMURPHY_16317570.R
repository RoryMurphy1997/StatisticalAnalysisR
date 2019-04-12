#Declare working directory
setwd("C:/Users/Cormac/Desktop/Statistical Analysis/9-Lab 9")

#Part 1
#Simulate micture model f(x) = (1/3)N(x,-1,0.1) + (1/3)N(x,0,0.1) + (1/3)N(x,1,0.1) for n=150
#Create vector of labels
labelVec <- ceiling(runif(150, min = 0, max = 3))
#Create vector of values
valuesVec <- vector(length = 150, "numeric")
#Values for (1/3)N(x,-1,0.1)
valuesVec[which(labelVec == 1)] <- rnorm(length(which(labelVec == 1)),mean = -1, sd = 0.1)
#Values for (1/3)N(x,0,0.1)
valuesVec[which(labelVec == 2)] <- rnorm(length(which(labelVec == 2)),mean = 0, sd = 0.1)
#Values for (1/3)N(x,1,0.1)
valuesVec[which(labelVec == 3)] <- rnorm(length(which(labelVec == 3)),mean = 1, sd = 0.1)
sampleVals <- data.frame(labels = labelVec, values = valuesVec)
#Plot histogram of results
hist(sampleVals$values, breaks = 150, freq = FALSE, main = "3 Component Mixture Model with equal s.d.")
#Can clearly see 3 normally distributed curves at means of -1, 0 and 1

#Vary standard deviation to 0.01, 0.1 and 1
#Create vector of labels
labelVecTwo <- ceiling(runif(150, min = 0, max = 3))
#Create vector of values
valuesVecTwo <- vector(length = 150, "numeric")
#Values for (1/3)N(x,-1,0.1)
valuesVecTwo[which(labelVecTwo == 1)] <- rnorm(length(which(labelVecTwo == 1)),mean = -1, sd = 0.01)
#Values for (1/3)N(x,0,0.1)
valuesVecTwo[which(labelVecTwo == 2)] <- rnorm(length(which(labelVecTwo == 2)),mean = 0, sd = 0.1)
#Values for (1/3)N(x,1,0.1)
valuesVecTwo[which(labelVecTwo == 3)] <- rnorm(length(which(labelVecTwo == 3)),mean = 1, sd = 1)
sampleValsTwo <- data.frame(labels = labelVecTwo, values = valuesVecTwo)
#Plot histogram of results
hist(sampleValsTwo$values, breaks = 150, freq = FALSE, main = "3 Component Mixture Model with unequal s.d.")
#Changing the standard deviations leads to differently shaped curves still centered around -1, 0 and 1
#If the standard deviation is sufficiently large, the different curves begin to merge together, making it impossible
#to identify different clusters for the data. Thus, as the standard deviation grows, it becomes more difficult to identify
#clusters.

#Part 2
#Import EMalg functions
source("EMalg.R")
#Gaussian mixture model density function
GMMDF <- function(EMOutput, xVals)
{
  #Determine number of groups to fit
  G <- EMOutput$G
  #Set up density vector
  densityVals <- vector(length = length(xVals), "numeric")
  #Determine density value for each given xVal
  for(i in 1:length(xVals))
  {
    #Initialise sum variable
    sum = 0
    #Calculate weighted value to be added to sum for each density function
    for(j in 1:G)
    {
      sum = sum + EMOutput$par$wei[j] * dnorm(xVals[i], mean = EMOutput$par$mu[j], sd = EMOutput$par$sd[j])
    }
    #Pass result to densityVals
    densityVals[i] = sum
  }
  #Return mixture density of given xVals
  return(densityVals)
}

#Create sequence of 500 evenly distributed values [-3,3]
xVals <- seq(from = -3, to = 3, length.out = 500)
EMOutput <- EM.GMM(x = sampleVals$values)
sampleDensities <- GMMDF(EMOutput,xVals)
#Create plot of values
plot(xVals, sampleDensities, type = "l", main = "Plot of Fitted Mixture Model")
#Overlay onto original histogram
#Original Histogram
hist(sampleVals$values, breaks = 150, freq = FALSE, main = "3 Component Mixture Model with equal s.d. and fitted mixture model")
#Overlay
points(xVals, sampleDensities, type = "l", lwd = 2, col = "red")

#Part 3
#Function to determine BIC from an EM.GMM list
EM.GMM.BIC <- function(EMOutput)
{
  #Number of parameters estimated
  m <- length(EMOutput$par$wei) + length(EMOutput$par$mu) + length(EMOutput$par$sd) - 1
  #Return BIC
  return(-2*EMOutput$loglike + m*log(length(EMOutput$data)))
}

#Fit model for various "G" with sd = 0.1
EMOutput2 <- EM.GMM(x = sampleVals$values, G = 2)
EM.GMM.BIC(EMOutput2)
EMOutput3 <- EM.GMM(x = sampleVals$values, G = 3)
EM.GMM.BIC(EMOutput3)
EMOutput4 <- EM.GMM(x = sampleVals$values, G = 4)
EM.GMM.BIC(EMOutput4)
EMOutput5 <- EM.GMM(x = sampleVals$values, G = 5)
EM.GMM.BIC(EMOutput5)
#BIC results
#2:238.6
#3:75.78
#4:82.91
#5:94.4
#Lowest BIC was with 3, the correct number of groups

#Fit model for various "G" with sd = 0.01, 0.1 and 1 respectively
EMOutput2 <- EM.GMM(x = sampleValsTwo$values, G = 2)
EM.GMM.BIC(EMOutput2)
EMOutput3 <- EM.GMM(x = sampleValsTwo$values, G = 3)
EM.GMM.BIC(EMOutput3)
#Cannot use mroe than 3 groupings as an error in EMaig occured
#BIC results
#2:-338.26
#3:-382.18
#Lowest BIC was with 2, could not identify the correct number of groupings due to the overlap occuring
#As the standard deviation increases, the groups begin to merge together. As with the graph where it became
#impossible to see the original groupings, the BIC cannot identify the correct number of groupings due to
#the groupings merging together.
