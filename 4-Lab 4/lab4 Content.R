#Data Visualisation

#Plot function
#Regression sample
x <- 1:100
y <- .1+.1*x + rnorm(100,0,1)

#par gives description of all plot parameters
?par

plot(x,y,pch = 20, cex = 1.5, cex.lab = 1.5, xlab = "X values", bty = "n", col = "dodgerblue", main = "My Plot", cex.main = 1.5)
#pch: plot symbols
#cex: magnify plot points
#xlab: set x axis label
#bty = "n": Remove box around plot

#Produce different colours for different statistics
X <- read.csv(file = "....csv")
Time <- X$time
Year <- X$year
plot(year, time)

#Create column of two colours
col1 <- "hotpink"
col2 <- "dodgerblue"

colourVec <- vector(length = nrow(X), "character")
colourVec [X$gender == 0] <- col1
colourvec [X$gender == 1] <- col2

#Plots each year and time combo with the relevant colour appearing in colourVec
plot(year, time, col = colourVec)

#Plot Types
#Line Plot
type = "l"
#Line and points Plot
type = "b"

#Points Function - Adding extra data to existing plot
z <- 0.1 + 0.2 * x + rnorm(100,0,1)
plot(x,y, ylim = c(min(y) - 1, max(z) + 1),pch = 20, cex = 1.5, cex.lab = 1.5, xlab = "X values", bty = "n", col = "dodgerblue", main = "My Plot", cex.main = 1.5)
#Can customize like before
points(x,z, cex = 1.5, pch = "*", col = "red", type = "b")
#Need to adjust xlim and ylim in original plot

#Histograms
#breaks = 25: Make 25 breaks along the x axis
#freq = FALSE: Turn into a desnity plot instead of a frequency plot
x <- rgamma(1000, shape = 2, rate = 1)
hist(x, freq = FALSE, ylim = c(0,max(density) + 0.1))
#Add a line of true distribution
#1001 items from 0 to 9
grid <- seq(0,9,length.out = 1001)
density <- dgamma(grid, shape = 2, rate = 1)
#lwd: Line width
points(grid, density, type = "l", lwd = 2, col = "red")

#Bar and Pie charts
#Need a vector of proportions for these
pie()
#labels: a column of names the same size as the number of entries appearing
#Can give a column of colours for the col argument
barplot()
#names.arg for the labels

#Display a number of graphs at once
par(mfrom = c(2,2)) #Create a 2X2 plotting matrix
#Each additional plot will be added to tile
#Need to reset to revert to 1 plot at a time
par(mfrow = c(1,1))