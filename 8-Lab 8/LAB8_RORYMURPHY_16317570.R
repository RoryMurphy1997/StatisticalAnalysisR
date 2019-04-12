#Declare working directory
setwd("C:/Users/Cormac/Desktop/Statistical Analysis/8-Lab 8")

#Part 1
#import hospital.dataset
hospital.data <- read.csv('med_care_demand.csv', header = TRUE, sep = "\t")
#Verify hospital.data is integer or factors
str(hospital.data)
levels(hospital.data$health)
#Graphs
fit <- glm(gp_visits~., data = hospital.data, family = poisson(log))
summary(fit)
summary(hospital.data$gp_visits)
#All factors were found to be significant
#Visualisations
#1: Hospital Stays
boxplot(log(gp_visits + 0.5)~hosp_stays, data = hospital.data, cex = 1.5, cex.lab = 1.5, xlab = "Number of Hospital Stays", ylab = "Log of number of GP Vists", col = "dodgerblue")
#2: Health Status
boxplot(log(gp_visits + 0.5)~health, data = hospital.data, cex = 1.5, cex.lab = 1.5, xlab = "Health Status", ylab = "Log of number of GP Vists", col = "dodgerblue")
#3: Number of Diagnosed Chronic Diseases
boxplot(log(gp_visits + 0.5)~number_chronic_diseases, data = hospital.data, cex = 1.5, cex.lab = 1.5, xlab = "Number of Diagnosed Chronic Diseases", ylab = "Log of number of GP Vists", col = "dodgerblue")
#4: Gender
boxplot(log(gp_visits + 0.5)~gender, data = hospital.data, cex = 1.5, cex.lab = 1.5, xlab = "Gender", ylab = "Log of number of GP Vists", col = "dodgerblue")
#5: Number of Years in Education
boxplot(log(gp_visits + 0.5)~school, data = hospital.data, cex = 1.5, cex.lab = 1.5, xlab = "Number of Years in Education", ylab = "Log of number of GP Vists", col = "dodgerblue")
#6: Does individual have private Insurance
boxplot(log(gp_visits + 0.5)~private_insurance, data = hospital.data, cex = 1.5, cex.lab = 1.5, xlab = "Has Health Insurance", ylab = "Log of number of GP Vists", col = "dodgerblue")
