#Declare working directory
setwd("C:/Users/Cormac/Desktop/Statistical Analysis/6-Lab 6")

#Part 1
#Generate simple linear regression values
xvalues <- 1:100
yvalues <- .1 + .2*x + rnorm(100,0,1)
#Create scatter plot
plot(x,y,pch = 20, xlab = "X values", ylab = "Y values", bty = "n", col = "dodgerblue", main = "Scatter Plot", cex.main = 1.5)

#Part 2
#Create function taking vector of regression parameters (theta) and a list of named entry for y (response) and x (explanetory) values returning least squares criterion
leastSquares <- function(theta, values)
{
  return(sum((values$y - theta[2]*values$x -theta[1])^2))
}

#Part 3
#Create list format of explanetory and response variables
variables <- list(x = xvalues, y = yvalues)
#Use 'optim' to estimate theta[0] and theta[1], the intercept and slope parameters, for the generated data in part 1
opt <- optim(par = c(1,1), fn = leastSquares, values = variables, method = "BFGS")
opt

#Part 4
#Repeat the simulation 1000 times and create kernel density estimate
#Initialise vector of parameter estimates
thetaZero <- vector(length = 1000)
thetaOne <- vector(length = 1000)
for(i in 1:1000)
{
  #Generate new simple linear regression values
  xvalues <- 1:100
  yvalues <- .1 + .2*x + rnorm(100,0,1)
  #Create list for function to use
  variables <- list(x = xvalues, y = yvalues)
  #Determine parameter estimates
  opt <- optim(par = c(1,1), fn = leastSquares, values = variables, method = "BFGS")
  #Add values to vectors
  thetaZero[i] <- opt$par[1]
  thetaOne[i] <- opt$par[2]
}
#Compute kenrel density estimates, allowing for +- 0.5 of the true values for the x axis to highlight the difference in variance
plot(density(thetaZero), xlim = c(-0.5,0.5))
plot(density(thetaOne), xlim = c(-0.3,0.7))
#Currently, estimate of intercept parameters are harder to accurately obtain. This can be seen by the larger interquartile range
#(3rd Qu.) - (1st Qu.) of the kernal density estimate of thetaZero compared to thetaOne.Thus, it can be said that the
# variance of the intercept parameter estimates is greater than that of the slope parameter estimates.

#A lower standard deviation of the normal deviates would decrease the deviation of our y value, resulting in 
#0.1 + 0,2x being a model that can more accurately predict values of y. Thus, for more samples, the maximum likelihood
#estimators of our simulated values would be closer to 0.1 and 0.2 for the intercept and slope respectively. For
#the density graphs, we would observe a decrease in the variance of both the slope and intercept parameter as each
#generated sample of linear regression is more likely to fit the 0.1 + 0.2x model.
