#Declare working directory
setwd("C:/Users/Cormac/Desktop/Statistical Analysis/3-Lab 3")
#Part 1
#Generate 1000*100 matrix of standard normal random variates.
X <- matrix(rnorm(1000 * 100, mean = 0, sd = 1), nrow = 1000, ncol = 100)

#Row means
rowMeans <- apply(X, MARGIN = 1, mean)
rowMeans

#Column means
colMeans <- apply(X, MARGIN = 2, mean)
colMeans

#Sum of squared row entries
#Create function
ssValue <- function(x)
{
  #Square vector values
  sq <- x^2
  #Sum and return the values
  return(sum(sq))
}
ssrows <- apply(X, MARGIN = 1, ssValue)

#Sum of exp(x) + 1 for each row
#Create function
sumExpPlusOne <- function(x)
{
  #Get exponentials + 1
  expon <- exp(x) + 1
  #Sum and return the values
  return(sum(expon))
}
sumExpPlusOneRows <- apply(X, MARGIN = 1,sumExpPlusOne)

#Part 2
#Function creates list of means and standard deviations of rows or columns
#bycols = 0 for rows, 1 for columns
meansAndSD <- function(x, bycols = 0)
{
  #If bycols is not 0 or 1, send error
  if(bycols != 0 && bycols != 1)
  {
    stop("bycols should be 0 or 1")
  }
  else
  {
    #Find means
    means <- apply(X, MARGIN = bycols + 1, mean)
    #Give names to means
    if(bycols == 0)
    {
      #Apply Row if bycols = 0
      names(means) <- paste("Row",1:length(means),"Mean")
    }
    else
    {
      #Apply Column if bycols = 1
      names(means) <- paste("Column",1:length(means),"Mean")
    }
    #Find standard deviations
    stdevs <- apply(X, MARGIN = bycols + 1, sd)
    #Generate names for st devs
    #Give names to st devs
    if(bycols == 0)
    {
      #Apply Row if bycols = 0
      names(stdevs) <- paste("Row",1:length(stdevs),"St Dev")
    }
    else
    {
      #Apply Column if bycols = 1
      names(stdevs) <- paste("Column",1:length(stdevs),"St Dev")
    }
    #Create return list
    ret <- list(means = means, stdevs = stdevs)
    return(ret)
  }
}
#Row values
ListOfMeansAndSDRows <- meansAndSD(X, 0)
ListOfMeansAndSDRows
#Column values
ListOfMeansAndSDRows <- meansAndSD(X, 1)
ListOfMeansAndSDRows


#Part 3
#Set histogram of row means
limits <- c(mean(rowMeans) - 3 * sd(rowMeans),mean(rowMeans) + 3 * sd(rowMeans))
hist(rowMeans, xlim = limits) 

#Generate 1000 * 1000 matrix of standard normal random variables
X2 <- matrix(rnorm(1000*1000, mean = 0, sd = 1), nrow = 1000, ncol = 1000)
#Find row means
rowMeans2 <- apply(X2, MARGIN = 1, mean)
hist(rowMeans2, xlim = limits)

#According to the central limits theorem, as n, the number of samples gets larger, the mean values of the samples 
#tend towards the population mean.Thus, for row means 1, since the sample of 1000 means were each built by only 100 
#standard normal random variables, the deviance of the resulting means is much larger than with the 1000 means built
# with 1000 standard normal random variables each. In other words, the standard error, which is computed by standard 
#deviation / sqrt(n) and gives the deviation of the sample means, is much smaller for the row means 2 than it is for
#row means one, leading to the same conclusion as part 1. This can be seen below

#Spread 1
spreadOne <- sd(rowMeans)/sqrt(length(rowMeans))
cat(spreadOne)
#Spread 2
spreadTwo <- sd(rowMeans2)/sqrt(length(rowMeans2))
cat(spreadTwo)

cat(spreadOne/spreadTwo)
#The spread of rowMeans1 is roughly 3.25 times larger than spread of rowMeans2 