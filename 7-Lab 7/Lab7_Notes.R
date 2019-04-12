#Generalised Linear Models
?glm
#Given data <- data.frame(X = x, Y = y)
fit <- glm(Y~X, data = data, family = binomial(logit))
summary(fit)
#Predictions of probability
predict(fit, data, type = "response")
#Data Cleaning: See Lab 7
#Always keep intercept estimate even if not significant
#For categories, need to identify the columns as factors when using glm or lm
x<-as.factor(x)
#Will output each factor except first (which is incorporated into intercept) and other factors will be in terms of intercept
#If any are significant, keep x
#Parameter of B1 for factors = intercept estimate + B1 estimate

