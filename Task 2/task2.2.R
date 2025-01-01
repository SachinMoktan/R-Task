# Load necessary libraries
library(MASS)

# Assuming the dataset is loaded into 'data' 
# data <- read.csv(""C:/Users/ZENBOOK/Desktop/R/Task 2/data_03cf4b0d-8941-4323-998c-a7ff1a83d0f2_1733812247131.csv"")

# Define input and output signals
X <- data[, c("x1", "x3", "x4", "x5")]
y <- data$x2

# Define the models
# Model 1: y = θ0 + θ1*x1 + θ2*x3 + θbias
model_1 <- lm(y ~ x1 + x3, data=data)

# Model 2: y = θ0 + θ1*x1^2 + θ2*x3^2 + θbias
model_2 <- lm(y ~ I(x1^2) + I(x3^2), data=data)

# Model 3: y = θ0 + θ1*x1^3 + θ2*x3^3 + θbias
model_3 <- lm(y ~ I(x1^3) + I(x3^3), data=data)

# Model 4: y = θ0 + θ1*x1 + θ2*x3 + θ3*x4^2 + θbias
model_4 <- lm(y ~ x1 + x3 + I(x4^2), data=data)

# Model 5: y = θ0 + θ1*x1 + θ2*x3 + θ3*x4^2 + θ4*x5^3 + θbias
model_5 <- lm(y ~ x1 + x3 + I(x4^2) + I(x5^3), data=data)

# Summary of each model
summary(model_1)
summary(model_2)
summary(model_3)
summary(model_4)
summary(model_5)

# Residual Sum of Squares (RSS) for each model
RSS1 <- sum(residuals(model_1)^2)
RSS2 <- sum(residuals(model_2)^2)
RSS3 <- sum(residuals(model_3)^2)
RSS4 <- sum(residuals(model_4)^2)
RSS5 <- sum(residuals(model_5)^2)

# Output RSS
list(
  Model_RSS1 = RSS1,
  Model_RSS2 = RSS2,
  Model_RSS3 = RSS3,
  Model_RSS4 = RSS4,
  Model_RSS4 = RSS5
)

