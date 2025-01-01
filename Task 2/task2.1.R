# Load necessary libraries
library(stats)

# Set working directory (optional, adjust path)
setwd("C:/Users/ZENBOOK/Desktop/R/Task 2")

# Load the dataset
data <- read.csv("C:/Users/ZENBOOK/Desktop/R/Task 2/data_03cf4b0d-8941-4323-998c-a7ff1a83d0f2_1733812247131.csv")

# Extract relevant columns
data <- data[, c("x1", "x2", "x3", "x4", "x5")]

# Define models
models <- list(
  model1 = lm(x2 ~ I(x4) + I(x1^2) + I(x1^3) + I(x2^4) + I(x1^4), data = data),
  model2 = lm(x2 ~ I(x4) + I(x1^3) + I(x3^4), data = data),
  model3 = lm(x2 ~ I(x3^3) + I(x3^4), data = data),
  model4 = lm(x2 ~ I(x2) + I(x1^3) + I(x3^4), data = data),
  model5 = lm(x2 ~ I(x4) + I(x1^2) + I(x1^3) + I(x3^4), data = data)
)

# Initialize results storage
results <- data.frame(Model = character(), SSE = numeric(), LogLikelihood = numeric(), AIC = numeric(), BIC = numeric())

# Iterate through models
for (i in 1:length(models)) {
  model <- models[[i]]
  model_name <- names(models)[i]
  
  # Residual Sum of Squares
  sse <- sum(residuals(model)^2)
  
  # Number of observations and parameters
  n <- nrow(data)
  k <- length(coefficients(model))
  
  # Variance of residuals
  sigma2 <- sse / n
  
  # Log-Likelihood
  log_likelihood <- -n/2 * log(2 * pi * sigma2) - sse / (2 * sigma2)
  
  # AIC and BIC
  aic <- 2 * k - 2 * log_likelihood
  bic <- k * log(n) - 2 * log_likelihood
  
  # Append results
  results <- rbind(results, 
                   data.frame(Model = model_name, 
                              SSE = sse, 
                              LogLikelihood = log_likelihood, 
                              AIC = aic, 
                              BIC = bic))
}

# Print results
print(results)

