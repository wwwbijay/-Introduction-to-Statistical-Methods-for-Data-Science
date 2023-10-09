# R program to illustrate
# Polynomial regression
#########################################
# Task 2 
# Polynomial Regression 
###########################################
# Importing required library
library(tidyverse)
library(caret)
library(ggplot2)

#scatter Plot
plot(X_1673241366257$V1, y_1673241374123$V1, main ="Polynomial Regression", las=1)

#model1 <- lm(y_1673241374123$V1 ~ X_1673241366257$V1)
#summary(model1)
#abline(model1, lwd = 3, col="red")


#model2 <- lm(y_1673241374123$V1 ~ X_1673241366257$V1 + I(X_1673241366257$V1^2))
#summary(model2)

#lines(smooth.spline(X_1673241366257$V1, predict(model2)), lwd = 3, col="blue")

y <- y_1673241374123$V1
x1 <- X_1673241366257$V1
x2 <- X_1673241366257$V2
x3 <- X_1673241366257$V3
x4 <- X_1673241366257$V4

model1 <- lm(y ~ x4 + I(x1^2) + I(x1^3) + I(x2^4) + I(x1^4))
summary(model1)

model2 <- lm(y ~ x4 + I(x1^3) + I(x3^4))
summary(model2)

model3 <- lm(y ~ x3^3 + I(x3^4))
summary(model3)

model4 <- lm(y ~ x2 + I(x1^3) + I(x3^4))
summary(model4)

model5 <- lm(y ~ x4 + I(x1^2) + I(x1^3) + I(x3^4))
summary(model5)

sum(resid(model1)^2)
sum(resid(model2)^2)
sum(resid(model3)^2)
sum(resid(model4)^2)
sum(resid(model5)^2)


logLik(model1)
logLik(model2)
logLik(model3)
logLik(model4)
logLik(model5)

AIC(model1)
BIC(model1)
AIC(model2)
BIC(model2)
AIC(model3)
BIC(model3)
AIC(model4)
BIC(model4)
AIC(model5)
BIC(model5)


# Data Partition
# load the libraries
library(caret)
library(klaR)
# load the dataset
data(customer_shopping_data)
# define an 70%/30% train/test split of the dataset
split=0.70
trainIndex <- createDataPartition(y_1673241374123$v1, p=split, list=FALSE)
#data_train <- iris[ trainIndex,]
#data_test <- iris[-trainIndex,]
# train a naive bayes model
#model <- NaiveBayes(Species~., data=data_train)
# make predictions
#x_test <- data_test[,1:4]
#y_test <- data_test[,5]
#predictions <- predict(model, x_test)
# summarize results
#confusionMatrix(predictions$class, y_test)




#########################################
# Task 3 ABC
# Rejection ABC 
###########################################
data <- y_1673241374123$V1
summary(data)


draw_mu <- function () {
  return (runif(1, min=0, max=10))
}
draw_sigma <- function () {
  return (runif(1, min=0, max=10))
}


simulate_data <- function (number_of_data_points, mu, sigma) { 
  return(rnorm(number_of_data_points, mean = mu, sd = sigma))
}

# We choose to use 3 quantiles.
compute_quantiles <- function(data) {
  return (quantile(data, probs=c(0.1, 0.5, 0.9)))
}
# First method to compare a simulated sample to the observed data
compare_quantiles_with_squared_distance <- function (true, simulated) {
  distance = sqrt(sum(mapply(function(x,y) (x-y)^2, true, simulated)))
  return(distance)
}

# Accept or reject based on the first method to compare a simulated sample to the observed data
accept_or_reject_with_squared_distance <- function (true, simulated, acceptance_threshold) {
  distance = compare_quantiles_with_squared_distance(compute_quantiles(true), compute_quantiles(simulated))
  if((distance < acceptance_threshold) ) return(T) else return(F)
}

sample_by_rejection <- function (true_data, n_iterations, acceptance_threshold, accept_or_reject_function) {
  number_of_data_points = length(true_data)
  accepted_or_rejected <- vector(length = n_iterations)
  sampled_mus <- vector(length = n_iterations, mode = "numeric")
  sampled_sigmas <- vector (length = n_iterations, mode = "numeric")
  for (i in 1:n_iterations){
    mu <- draw_mu()
    sigma <- draw_sigma()
    parameters = list("mu"=mu, "sigma"=sigma)
    simulated_data <- simulate_data(number_of_data_points, mu, sigma)
    accepted_or_rejected[i] = accept_or_reject_function(true_data, simulated_data, acceptance_threshold)
    sampled_mus[i] = mu
    sampled_sigmas[i] = sigma
  }
  return(data.frame(cbind("accepted_or_rejected" = accepted_or_rejected, "sampled_mus" = sampled_mus, "sampled_sigmas" = sampled_sigmas)))
}

sampled_parameter_values_squared_distances = sample_by_rejection(data, 200000, 0.5, accept_or_reject_with_squared_distance)

sum(sampled_parameter_values_squared_distances$accepted_or_rejected)


library(coda)
rej_samples_squared_distances_as_mcmc = mcmc(sampled_parameter_values_squared_distances[which(sampled_parameter_values_squared_distances$accepted_or_rejected==1),c(2,3)])

summary(rej_samples_squared_distances_as_mcmc)


plot(rej_samples_squared_distances_as_mcmc)




