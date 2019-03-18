#Generalised Linear Models
?glm
#Given data <- data.frame(X = x, Y = y)
fit <- glm(Y~X, data = data, family = binomial(logit))
summary(fit)
#Predictions of probability
predict(fit, data, type = "response")