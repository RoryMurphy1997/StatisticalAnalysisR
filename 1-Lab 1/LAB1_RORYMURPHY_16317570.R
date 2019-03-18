#Part 1
?setwd
#Set working directory to the folder this file is in
setwd("C:/Users/Cormac/Desktop/Statistical Analysis/1-Lab 1")

#Part 2
#Clauses
direct1 <- c("You can eat","You should not eat")
direct2 <- c("cat food", "salmon", "chicken", "eggs", "hard drives", "monitors", "keyboards", "cardboard")

#Part 3
#Clause 1
direct1[1]
#Clause 2
direct1[2]

#Part 4
#Examine "Paste" method
str <- paste(direct1[2],direct2[2])
cat(str, "\n")

#Part 5
#Generate direct1 selection
id1 <- sample(1:length(direct1), size = 1, replace=TRUE)

#Random Advice
if(id1 == 1)
{
  #Generate direct2 selection for "You can eat"
  id2 <- sample(c(2,3,4), size=1, replace=TRUE)
}else{
  #Generate direct2 selection for "You should not eat"
  id2 <-sample(c(1,5,6,7,8),size=1,replace=TRUE)
}
#Create random advice
advice <- paste(direct1[id1],direct2[id2])
#Print generated advice
cat(advice,"\n")

#Part 6
#Generate 100 pieces of advice and print them to the screen
for(i in 1:100)
{
  #Generate direct1 selection
  id1 <- sample(1:length(direct1), size = 1, replace=TRUE)
  if(id1 == 1)
  {
    #Generate direct2 selection for "You can eat"
    id2 <- sample(c(2,3,4), size=1, replace=TRUE)
  }else{
    #Generate direct2 selection for "You should not eat"
    id2 <-sample(c(1,5,6,7,8),size=1,replace=TRUE)
  }
  #Create random advice
  advice <- paste(direct1[id1],direct2[id2])
  #Print generated advice
  cat(advice,"\n")
}