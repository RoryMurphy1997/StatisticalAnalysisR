#pasteo will remove whitespace for filenames

#Function
myfunc <- function(arg1, arg2)
{
  #Do the thing
  return()
}

#To look at info about function, run it without brackets or arguments

#Numeric
X <- rnorm(100, mean = 0, sd = 1)
#Labelling
Y <- sample(1:5, size = 100, replace = TRUE)

#Central Tendancy
mean(X)
median(X)
#Spread
sd(X)
range(X)
IQR(X) #Interquartile range
summary(X) #Can be used to identify any oddities in data

#Frequency Based Summary
#FrequencyTable
table(Y)
prop.table(table(Y)) #Create proportion table
prop.table(table(Y)) * 100 #% values

#apply
X2 <- matrix(X, nrow=10, ncol = 10)
#applies the mean function to each row
apply(X2, MARGIN = 1, mean)
#Finds index of biggest item in each column
apply(X2, MARGIN = 2, which.max)

#Function to get ratio of numbers 1 and 2 to 3
ratio <- function(n1, n2, n3 = 1)
{
  #Check for valid n3
  if(n3 == 0)
  {
    warning("n3 should not be 0")
    #Could use stop("") either, which wont even run code
  }
  ratio1 <- n1/n3
  ratio2 <- n2/n3
  #Create list of returned values
  ret <- list(ratio = c(ratio1, ratio2), denominator = n3)
  return(ret)
}
ratio(50, 25, 5)

#Take a vector and filename and create histogram of vector putting it in PDF of filename
posthist <- function(x, filename = "MyHistogram.pdf")
{
  hist(x)
  dev.copy2pdf(file = filename)
}
posthist(X, "Histogram.pdf")

#Useful Functions
?NA # Help page on handling missing data values
abs(x) #absolute value
append() #Add an element to a vector
c(x) #combine its arguments
cat(x) #print the arguments
cbind() #Combine vectors by columns (rbind = rows)
diff(x) #Return suitably lagged differeces
gre() #pattern matching
identical() #Test if 2 objects are exactly the same
jitter() #Add a small amount of noise to a vector
length(x) #no of elements in x
ls() #list all objects in current environment
paste(x) #concatenate character vectors
range(x) #min and max of x
rep(1,5) #repeat the number 1 five times
rev(x) #Reverse the elements of x
seq(1,10,04) #Sequence of 1 through 10 incremented by 0.4
sign(x) #Return signs of elements in x
sort(x) #Sort the vector x
order(x) #Return elemenets of x in order
tolower()/toupper #convert string to lower/upper case
unique(x) #removes duplicate entries of x
vector() #create a vector of given length and mode

#Mathematical
log(),logb(),log10(),log2(),exp(),expm1(),log1p(),sqrt()
cos(),sin(),tan(),acos(),asin(),atan(),atan2()
cosh(),sinh(),tanh(),acosh(),asinh(),atanh()
union(),intersect(),setdiff(),setequal()
eigen() # eigenvalues and eigenvectors
