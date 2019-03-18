#Declare working directory
setwd("C:/Users/Cormac/Desktop/Statistical Analysis/4-Lab 4")

#Part 1
#Extract data from FTSE_1.csv
Spot_Price_Data <- read.table(file = "FTSE_1.csv", header = TRUE, sep = ",")
#Remove ':' from time data
Spot_Price_Data$DateTime = as.numeric(gsub("\\:", "", Spot_Price_Data$DateTime))
#Find data between 11am and 12pm 
index <- which(Spot_Price_Data$DateTime >= 110000 & Spot_Price_Data$DateTime <= 120000)
#Create new data.frame to store values in
Hour_Spot_Price <- list()
#Add datetimes between 11am and 12pm
Hour_Spot_Price$DateTime <- Spot_Price_Data[index,1]
#Add corresponding spot prices for datetimes between 11am and 12pm
Hour_Spot_Price$Spot <- Spot_Price_Data[index,2]
#Reformat datetimes for graph
Hour_Spot_Price$DateTime <- ((Hour_Spot_Price$DateTime - 110000)/100) * 60
#Create visualisation using required layout
plot(1:length(Hour_Spot_Price$Spot), Hour_Spot_Price$Spot, xlab = "seconds after 11:00:00", ylab = "spot", col = "red", lty = 3, main = "Trading hr 11:00-12:00", type = "l", cex.lab = 1.3 )
#Add mean Spot price line
abline(h = mean(Hour_Spot_Price$Spot),lty = 2,col = "blue", lwd = 1.5)
#Produce PDF "trading_hr_110000.pdf"
dev.copy2pdf(file = "trading_hr_110000.pdf")

#Part 2
#Extract data from "coordinates.txt"
coords <- read.table(file = "coordinates.txt", header = TRUE)
coords
#Extract data from "labels.txt"
labels <- read.table(file = "labels.txt", header = FALSE)
#Create plot of coordinates based on which pack the wolf was in
#Select colours for the two packs
colour_one <- "red"
colour_two <- "blue"
#Create a vector of colours where blue occurs when a wolf from pack 1 was sighted and red for pack 2
colour_vec <- vector(length = nrow(labels), "character")
colour_vec [labels == 1] <- colour_one
colour_vec [labels == 2] <- colour_two
#Make plot square
par(pty="s")
#Graph visualisation using these colours for corresponding sightings
plot(coords$horizontal, coords$vertical, col = colour_vec, xlab = "lat", ylab = "lon", main = "sightings from base", pch = 20)
#Produce PDF "wolves.pdf"
dev.copy2pdf(file = "wolves.pdf")
