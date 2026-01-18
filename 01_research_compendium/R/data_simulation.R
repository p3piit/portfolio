# ===============================================================
# In this script we simulate the data that will be used for the anaLysis
# in the report.
# ===============================================================
# clean the environment
rm(list = ls())

# Load necessary libraries
library(mlml) 
# This is my package with all the costum functions that I use and it
# also imports all the other necessary libraries.

# ================================================================
# Function to simulate hierarchical binary data with random intercepts and slope,
# for the first simulation scenario.
sim_data1 <- function(G = 50,          # number of clusters
                      n_i = 40,        # observations per cluster
                      beta0 = 0.5,     # fixed intercept
                      beta1 = 1.2,     # fixed effect for x1
                      beta2 = -0.8,    # fixed effect for x2
                      beta3 = 0.6,     # fixed effect for x3
                      sigma_b0 = 0.8,  # SD of random intercept
                      sigma_b1 = 0.5,  # SD of random slope for x1
                      rho = 0.2,       # correlation between b0 and b1
                      seed = 123) {    # random seed for reproducibility
  
  set.seed(seed)
  
  # covariance matrix of random effects (b0_i, b1_i)
  D <- matrix(c(sigma_b0^2, rho * sigma_b0 * sigma_b1,
                rho * sigma_b0 * sigma_b1, sigma_b1^2), 2, 2)
  
  # generate random effects for G clusters
  b <- MASS::mvrnorm(G, mu = c(0, 0), Sigma = D)
  
  # cluster identifiers
  id <- rep(1:G, each = n_i)
  
  # generate covariates: x1 (uniform), x2 (normal), x3 (binary)
  x1 <- runif(G * n_i, -2, 2)
  x2 <- rnorm(G * n_i, 0, 1)
  x3 <- rbinom(G * n_i, 1, 0.5)
  
  # linear predictor: fixed part + random part (b0_i + b1_i * x1)
  eta <- numeric(G * n_i)
  for (g in seq_len(G)) {
    idx <- which(id == g)
    eta[idx] <- beta0 + beta1 * x1[idx] + beta2 * x2[idx] + beta3 * x3[idx] +
      b[g, 1] + b[g, 2] * x1[idx]
  }
  
  # convert to probabilities via logistic link
  p <- 1 / (1 + exp(-eta))
  
  # binary outcome drawn from Bernoulli(p)
  y <- rbinom(G * n_i, 1, p)
  
  # return as data.frame ready for model fitting
  data.frame(
    id = factor(id),
    y = y,
    x1 = x1,
    x2 = x2,
    x3 = x3
  )
}

# ================================================================
# Function to simulate hierarchical binary data with random intercepts and slope,
# for the second simulation scenario. Here x2 has a log transformation in the linear predictor.
sim_data2 <- function(G = 50,          # number of clusters
                      n_i = 40,        # observations per cluster
                      beta0 = 0.5,     # fixed intercept
                      beta1 = 1.2,     # fixed effect for x1
                      beta2 = -1,    # fixed effect for x2
                      beta3 = 0.6,     # fixed effect for x3
                      sigma_b0 = 0.8,  # SD of random intercept
                      sigma_b1 = 0.5,  # SD of random slope for x1
                      rho = 0.2,       # correlation between b0 and b1
                      seed = 123) {    # random seed for reproducibility
  
  set.seed(seed)
  
  # covariance matrix of random effects (b0_i, b1_i)
  D <- matrix(c(sigma_b0^2, rho * sigma_b0 * sigma_b1,
                rho * sigma_b0 * sigma_b1, sigma_b1^2), 2, 2)
  
  # generate random effects for G clusters
  b <- MASS::mvrnorm(G, mu = c(0, 0), Sigma = D)
  
  # cluster identifiers
  id <- rep(1:G, each = n_i)
  
  # generate covariates: x1 (uniform), x2 (normal), x3 (binary)
  x1 <- runif(G * n_i, -2, 2)
  x2 <- rnorm(G * n_i, 5, 1)
  x3 <- rbinom(G * n_i, 1, 0.5)
  
  # linear predictor: fixed part + random part (b0_i + b1_i * x1)
  eta <- numeric(G * n_i)
  for (g in seq_len(G)) {
    idx <- which(id == g)
    eta[idx] <- beta0 + beta1 * x1[idx] + beta2 * log(x2[idx]) + beta3 * x3[idx] +
      b[g, 1] + b[g, 2] * x1[idx]
  }
  
  # convert to probabilities via logistic link
  p <- 1 / (1 + exp(-eta))
  
  # binary outcome drawn from Bernoulli(p)
  y <- rbinom(G * n_i, 1, p)
  
  # return as data.frame ready for model fitting
  data.frame(
    id = factor(id),
    y = y,
    x1 = x1,
    x2 = x2,
    x3 = x3,
    eta = eta
  )
}

# ================================================================
# Function to simulate hierarchical binary data with random intercepts and slope,
# for the third simulation scenario. Here there is an interaction between x2 and x4 in the linear predictor.
sim_data3 <- function(G = 50,          # number of clusters
                      n_i = 40,        # observations per cluster
                      beta0 = 0.5,     # fixed intercept
                      beta1 = 1.2,     # fixed effect for x1
                      beta2 = -0.8,    # fixed effect for x2
                      beta3 = 0.6,     # fixed effect for x3
                      sigma_b0 = 0.8,  # SD of random intercept
                      sigma_b1 = 0.5,  # SD of random slope for x1
                      rho = 0.2,       # correlation between b0 and b1
                      seed = 123) {    # random seed for reproducibility
  
  set.seed(seed)
  
  # covariance matrix of random effects (b0_i, b1_i)
  D <- matrix(c(sigma_b0^2, rho * sigma_b0 * sigma_b1,
                rho * sigma_b0 * sigma_b1, sigma_b1^2), 2, 2)
  
  # generate random effects for G clusters
  b <- MASS::mvrnorm(G, mu = c(0, 0), Sigma = D)
  
  # cluster identifiers
  id <- rep(1:G, each = n_i)
  
  # generate covariates: x1 (uniform), x2 (normal), x3 (binary)
  x1 <- runif(G * n_i, -2, 2)
  x2 <- rnorm(G * n_i, 5, 1)
  x3 <- rbinom(G * n_i, 1, 0.5)
  x4 <- rbinom(G * n_i, 1, 0.5)
  
  # linear predictor: fixed part + random part (b0_i + b1_i * x1)
  eta <- numeric(G * n_i)
  for (g in seq_len(G)) {
    idx <- which(id == g)
    eta[idx] <- beta0 + beta1 * x1[idx] + beta2 * x2[idx]*x4[idx] + beta3 * x3[idx] +
      b[g, 1] + b[g, 2] * x1[idx]
  }
  
  # convert to probabilities via logistic link
  p <- 1 / (1 + exp(-eta))
  
  # binary outcome drawn from Bernoulli(p)
  y <- rbinom(G * n_i, 1, p)
  
  # return as data.frame ready for model fitting
  data.frame(
    id = factor(id),
    y = y,
    x1 = x1,
    x2 = x2,
    x3 = x3,
    x4 = x4
  )
}

# ================================================================
# Function to simulate hierarchical binary data with random intercepts and slope,
# for the fourth simulation scenario. Here the predictors are correlated.
sim_data4 <- function(G = 50,          # number of clusters
                      n_i = 40,        # observations per cluster
                      beta0 = 0.5,     # fixed intercept
                      beta1 = 1.2,     # fixed effect for x1
                      beta2 = -0.8,    # fixed effect for x2
                      beta3 = 0.6,     # fixed effect for x3
                      sigma_b0 = 0.8,  # SD of random intercept
                      sigma_b1 = 0.5,  # SD of random slope for x1
                      rho = 0.2,       # correlation between b0 and b1
                      seed = 123) {    # random seed for reproducibility
  
  set.seed(seed)
  
  # covariance matrix of random effects (b0_i, b1_i)
  D <- matrix(c(sigma_b0^2, rho * sigma_b0 * sigma_b1,
                rho * sigma_b0 * sigma_b1, sigma_b1^2), 2, 2)
  
  # generate random effects for G clusters
  b <- MASS::mvrnorm(G, mu = c(0, 0), Sigma = D)
  
  # cluster identifiers
  id <- rep(1:G, each = n_i)
  
  # --- Create correlated predictors ---
  N <- G * n_i
  Sigma_X <- matrix(c(1, 0.9, 0.7,
                      0.9, 1, 0.6,
                      0.7, 0.6, 1), ncol = 3)
  X <- MASS::mvrnorm(N, mu = c(0, 0, 0), Sigma = Sigma_X)
  x1 <- X[,1]
  x2 <- X[,2]
  x3 <- ifelse(X[,3] > 0, 1, 0)
  
  # linear predictor: fixed part + random part (b0_i + b1_i * x1)
  eta <- numeric(N)
  for (g in seq_len(G)) {
    idx <- which(id == g)
    eta[idx] <- beta0 + beta1 * x1[idx] + beta2 * x2[idx] + beta3 * x3[idx] +
      b[g, 1] + b[g, 2] * x1[idx]
  }
  
  # convert to probabilities via logistic link
  p <- 1 / (1 + exp(-eta))
  
  # binary outcome drawn from Bernoulli(p)
  y <- rbinom(N, 1, p)
  
  # return as data.frame ready for model fitting
  data.frame(
    id = factor(id),
    y = y,
    x1 = x1,
    x2 = x2,
    x3 = x3
  )
}

# ================================================================
# Simulate data for the first scenario
df1 <- sim_data1(G = 30, n_i = 60)
write.csv(df1, file = "data/simulated_data1.csv", row.names = FALSE)
# Simulate data for the second scenario
df2 <- sim_data2(G = 30, n_i = 60)
write.csv(df2, file = "data/simulated_data2.csv", row.names = FALSE)
# Simulate data for the third scenario
df3 <- sim_data3(G = 30, n_i = 60)
write.csv(df3, file = "data/simulated_data3.csv", row.names = FALSE)
# Simulate data for the fourth scenario
df4 <- sim_data4(G = 30, n_i = 60)
write.csv(df4, file = "data/simulated_data4.csv", row.names = FALSE)
# ================================================================

