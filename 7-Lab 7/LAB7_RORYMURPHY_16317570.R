#Declare working directory
setwd("C:/Users/Cormac/Desktop/Statistical Analysis/7-Lab 7")

#Part 1
#Read in breast cancer dataset
data <- read.table('breast_cancer.dat', header = TRUE, sep = ",")
#Remove ID Number from data
data <- data[,-1]

#Remove any rows with missing data
#Find rows where '?' appears
ind <- which(data == '?', arr.ind = TRUE)[,1]
#Remove these rows
dataCleaned <- data[-ind,]
#Ensure all values are numeric
str(dataCleaned)
#Convert to character first
dataCleaned$bare_nuclei <- as.character(dataCleaned$bare_nuclei)
#Convert "bare_nuclei" column to "numeric"
dataCleaned$bare_nuclei <- as.numeric(dataCleaned$bare_nuclei)

#Visualisation of class relative to measurements
#Clump Thickness scores appear larger for malignant tumors than benign tumors, making it likely to be a significant
#measurement in determining if a tumor is malignant or benign.
boxplot(clump_thick~class_tumor,dataCleaned, xlab = "Class of Tumor", ylab = "Clump Thickness")

#Uniformity of Cell Size scores appear larger for malignant tumors than benign tumors, making it likely to be a significant
#measurement in determining if a tumor is malignant or benign.
boxplot(unif_cell_size~class_tumor,dataCleaned, xlab = "Class of Tumor", ylab = "Uniformity of Cell Size")

#Uniformity of Cell Shape scores appear larger for malignant tumors than benign tumors, making it likely to be a 
#significant measurement in determining if a tumor is malignant or benign.
boxplot(unif_cell_shape~class_tumor,dataCleaned, xlab = "Class of Tumor", ylab = "Uniformity of Cell Shape")

#Marginal Adhesion scores appear larger for malignant tumors than benign tumors, making it likely to be a significant
#measurement in determining if a tumor is malignant or benign.
boxplot(marg_adhesion~class_tumor,dataCleaned, xlab = "Class of Tumor", ylab = "Marginal Adhesion")

#Single Epithelial Cell Size scores appear to be only somewhat larger for malignant tumors than benign tumors. This
#makes it less likely to be a significant factor in determining whether a tumor is malignant or benign
boxplot(epithelial_size~class_tumor,dataCleaned, xlab = "Class of Tumor", ylab = "Single Epithelial Cell Size")

#Bare Nuclei scores appear only somewhat larger for malignant tumors than benign tumors. This makes it less likely to 
#be a significant factor in determining whether a tumor is malignant or benign
boxplot(bare_nuclei~class_tumor,dataCleaned, xlab = "Class of Tumor", ylab = "Bare Nuclei")

#Bland Chromatin scores appear larger for malignant tumors than benign tumors, making it likely to be a 
#significant measurement in determining if a tumor is malignant or benign.
boxplot(bland_chromatin~class_tumor,dataCleaned, xlab = "Class of Tumor", ylab = "Bland Chromatin")

#Normal Nucleoli scores appear larger for malignant tumors than benign tumors, making it likely to be a 
#significant measurement in determining if a tumor is malignant or benign.
boxplot(normal_nucleoli~class_tumor,dataCleaned, xlab = "Class of Tumor", ylab = "Normal Nucleoli")

#For Mitosis, the median scores to not appear to differ much if at all. This makes it less likely to be a significant
#measurement in determining whether a tumor is malignant or benign.
boxplot(mitoses~class_tumor,dataCleaned, xlab = "Class of Tumor", ylab = "Mitosis")

#Convert "class_tumor" to 0 and 1 for use with "glm" (since it doesn't run with '2' and '4')
dataCleaned$class_tumor <- dataCleaned$class_tumor/2 - 1

#Generalised Linear Model: Logistic Regresion Model to Predict Benign or Malignant
eq <- class_tumor~clump_thick+unif_cell_size+unif_cell_shape+marg_adhesion+epithelial_size+bare_nuclei+bland_chromatin+normal_nucleoli
model <- glm(eq, data = dataCleaned, family = binomial(logit))
summary(model)
#Significant variables (Based on 5% level of significance)
#Intercept: -10.6696 -> With no other information, a given tumor is more likely to be benign
#Clump Thickness: 0.6559 -> Tumors with larger scores on clump thickness are more likely to be malignant
#Uniformity of Cell Shape: 0.4696 -> Tumors with larger scores on uniformity of cell shape are more likely to be malignant
#Marginal Adhesion: 0.3879 -> Tumors with larger scores on marginal adhesion are more likely to be malignant
#Bland Chromatin: 0.6168 -> Tumors with larger scores on bland chromation are more likely to be malignant

#Rebuild model with variables that aren't significant in the model removed
eqtwo <- class_tumor~clump_thick+unif_cell_shape+marg_adhesion+bland_chromatin
modeltwo <- glm(eqtwo, data = dataCleaned, family = binomial(logit))
summary(modeltwo)
#Model two gave approximately the same intercept value (-10.0873 here) and all four variables were found to be 
#significant, with roughly similar coefficients estimated again. It should be noted that Pr(>|z|) (or likelihood of true
#coefficient for this variable being 0 given the data results) was much smaller for the variables measured in this model.
#For the intercept, Pr(>|z|) was the same for both models

#The significance observed by the generalised linear model generally matches the box plots which had larger differences
#in their  medians, with only "Uniformity of Cell Size" and "Normal Nucleoli" having larger differences in their values
#shown by the box plots. 

