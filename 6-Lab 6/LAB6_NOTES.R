#optim function
#Finds local min (or max if given negative version of function) so use multiple starting points to avoid

#Example 1: minimize f(x) = (x-2)^2
#Declare function
func.one <- function(x)
{
  return((x-2)^2)
}
#Run optiom par = starting value, fn = function, method = "BFGS"
op <- optim(par = 1, fn = func.one, method = "BFGS")
#Return values: par (vector of optimal values), value = f(par), counts = Number of iterations needed, converse should = 0
#Message = Any errors encountered

#Example 2: Multivariate: maximum g(x,y) = 2-x^2-y^2
#Give negative function to find max
#Input vector of two values
func.two <- function(x)
{
  return(-2+x[1]^2+x[2]^2)
}
optwo <- optim(par = c(1,1), fn = func.two, method = "BFGS")

#Knowing the gradient
#If you pass the first derivate to the model, becomes more accurate
func.two.grad <- function(x)
{
  return(c(2*x[1], 2*x[2]))
}
opthree <- optim(par = c(1,1), fn = func.two, gr = func.two.grad, method = "BFGS")
#Reduces $counts

#Additional Arguments
#Suppose function has extra arguments you might want to pass
#Ex: 2-x^2-y^2-v
func.three <- function(x,v)
{
  return(-2+x[1]^2+x[2]^2+v)
}
func.three.grad <- function(x,v)
{
  return(c(2*x[1], 2*x[2]))
}
#Make sure to put v = after function and gradient functions
opfour <- optim(par = c(1,1), fn = func.three, gr = func.three.grad, v = 3, method = "BFGS")