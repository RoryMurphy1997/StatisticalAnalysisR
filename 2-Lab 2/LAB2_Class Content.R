#Vectors
#Empty vector of size 10
X <- numeric(10)
#Vector with 3 items
X <- c(3,6,9)
#Second item of X
X[2]
age <- c(63,67,72,74)
weight <- c(67.2, 83.1, 75.5, 76.5)
county <- c("Dublin", "Meath", "Dublin", "Kilkenny")
#All ages except first
age[-1]
#or
age[2:4]
#Vector Data Type
typeof(age)
typeof(county)

#Matrix
X <- matrix(nrow = 4, ncol = 2)
#Item in first row and second column
X[1,2]
#Row 2
X[2,]
#Col 2
X[,2]
#Give columns data
X[,1] <- age
X[,2] <- weight
#Find all with age over 70
idx <- which(X[,1] >70)
Xnew <- X[idx,]
Xnew
#Or
colnames(X) <- c("age","weight")
subset(X,age>70)
#Age >60 and weight >76
subset(X,age > 60 & weight > 76)

#Data Frames
D <- data.frame(age = age, weight = weight, county = county)
#Access list of ages
D$age
#Access rows with county = Dublin
subset(D,county == "Dublin")
D[1,3]

#Exercise
D <- data.frame(age = age, weight = weight, county = county)
#Query DB for all participants outside Dublin
notDub <- subset(D,county != "Dublin")
val <- mean(notDub$age)
cat("\n", val)

#Reading and Writing datasets
#Make csv
write.table(D, file = "MyData.csv", row.names = FALSE, sep = ",")
?write.table
Y <- read.table(file = "MyData.csv", header = TRUE, sep = ",")
Y <- read.csv(file = "MyData.csv", header = TRUE)
?read.table
#Seperators tab: "\t", space: " "
#Factors
Y$county
levels(Y$county)
#Change all appearences of Dublin to Cavan
levels(Y$county)[1] <- "Cavan"
Y

#Lists
#Add 2 to all ages
Y$age <- Y$age + 2
Y$age

