#Declare working directory
setwd("C:/Users/Cormac/Desktop/Statistical Analysis/10-Lab 10")

#Q1
#alphabet column
alphabet <- letters[seq( from = 1, to = 26 )]
#numbers column
numbers <- seq(from = 1, to = 26)
#alphanumbers column
alphabetNumbers <- as.vector(26)
for(i in 1:26)
{
  alphabetNumbers[i] <- paste(paste0(alphabet[i], "_", numbers[i]))
}
#Create data frame
questionOne <- data.frame(alphabet = alphabet, numbers = numbers, alphanumbers = alphabetNumbers)
questionOne

#Q2
#Simulate values
normalMatrix <- matrix(rnorm(100*100),nrow = 100, ncol = 100)
#Print each column to a file
for(i in 1:100)
{
  name <- paste(paste0("column_number_",i,".txt"))
  write.table(normalMatrix[,i], file = name)
}

#Q3
#Create function
functionOne <- function(x)
{
  return(3*x*exp(-x)+3)
}
values <- seq(from = 0, to = 10)
#Plot values
plot(values,functionOne(values))

#Q4
#Neg version of function
functionTwo <- function(x)
{
  return(-1*(-3*(x^2)+2*(x-10)*sin(x-0.75)))
}
#Find minimum of negative function (or max of positive function)
op <- optim(par = 1, fn = functionTwo, method = "BFGS")
#Answer = 
cat(op$par)

#Q5
#Import data
five <- read.table(file = "users.dat", header = FALSE)
#Number of different users
cat(length(levels(five$V1)),"\n")
#Find maximum number of uses by a user and the user's ID
maxUse <- 0
maxUser <- ""
for(i in 1:length(levels(five$V1)))
{
  use <- length(which(five$V1 == levels(five$V1)[i]))
  if (use > maxUse)
  {
    maxUse = use
    maxUser = levels(five$V1)[i]
  }
}
#User with most accesses
cat(maxUser,"\n")
#Number of uses
cat(maxUse)

#Q6
#Import data
six <- read.table(file = "age_value_dat.csv", header = TRUE, sep = ",")
#Change age ranges
levels(six$age_bracket)[1:2] <- "Up to 24 yr"
levels(six$age_bracket)[3:5] <- "35 yr and above"
#Create boxlot
#Make plot square
par(pty="s")
boxplot(value~age_bracket, six, pch = "+")
#Reset par
par(pty = "m")

#Q7
#Suits
suit <- c("Heart", "Club", "Diamond", "Clove")
#Values
cardVals <- c("A", "K", "Q", "J", 2,3,4,5,6,7,8,9,10)
#Create deck
deck <- as.vector(52)
#Set card count
cardNumber <- 1
#For each suit
for(i in 1:4)
{
  #For each possible value in each suit
  for(j in 1:13)
  {
    #Create a card in the deck
    deck[cardNumber] <- paste(paste0(suit[i], " ", cardVals[j]))
    #Increment card count
    cardNumber <- cardNumber + 1
  }
}
#Sample 5 cards fromd deck at random
hand <- sample(deck,5)
#Print the cards
for(i in 1:5)
{
  cat(hand[i],"\n")
}


