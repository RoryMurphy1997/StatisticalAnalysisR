#Finite Mixture Models
#Clustering Algorithms
kmeans() #Non-Parametric
mclust() #Multivariate continuous data
BayesLCA() #Multivariate Categorical data
clustMD() #Multivariate mixed data

#Tomorrow: use EMaig to simulate data with some parameters then find said parameters again

#Generate values:Generate n labels proportional to prior probabilities, then for each value, generate from the corresponding
#normal distribution

#EG: f(x) = 0.5N(x,-3,1) + 0.5N(x,3,1)
labelVec <- vector(length = 100, "character")
labelVec [1:50] <- 'Label 1'
labelVec [51:100] <- 'Label 2'
valuesVec <- vector(length = 100, "numeric")
valuesVec[which(labelVec == 'Label 1')] <- rnorm(length(which(labelVec == 'Label 1')),mean = -3, sd = 1)
valuesVec[which(labelVec == 'Label 2')] <- rnorm(length(which(labelVec == 'Label 2')),mean = 3, sd = 1)
valuesVec
sampleVals <- data.frame(labels = labelVec, values = valuesVec)
sampleVals
#Estimate Parameters (no initial values)
EM.GMM(sampleVals$values, G = 2)
#Estimate Parameters (with initial values)
initialVals <- data.frame(wei = c(0.5,0.5), mean = c(-3,3), var = c(1,1))
EM.GMM(sampleVals$values, G = 2, initpar = initialVals)
