# Set the parameters
S0 <- 100       # Initial stock price
sigma <- 0.2    # Volatility
r <- 0.03       # True drift (the drift coefficient you want to estimate)
T <- 1          # Time horizon in years
n <- 252        # Number of time steps (daily data for a year)
dt <- T / n     # Time step

# Number of simulations
num_simulations <- 1000

# Create an empty matrix to store the simulated prices
simulated_prices <- matrix(0, nrow = n, ncol = num_simulations)

# Perform the Monte Carlo simulations
set.seed(123) # Set a seed for reproducibility

for (j in 1:num_simulations) {
  prices <- numeric(n + 1)
  prices[1] <- S0
  
  for (i in 1:n) {
    z <- rnorm(1) # Generate a standard normal random variable
    S_t <- prices[i] * exp((r - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * z)
    prices[i + 1] <- S_t
  }
  
  simulated_prices[, j] <- prices[-1]
}

# Calculate the drift coefficient for each simulation
estimated_drift <- numeric(num_simulations)

for (j in 1:num_simulations) {
  log_returns <- log(simulated_prices[, j] / S0)
  estimated_drift[j] <- mean(log_returns) / dt
}

# Plot a histogram of the estimated drift coefficients
hist(estimated_drift, main = 'Histogram of Estimated Drift Coefficients', xlab = 'Estimated Drift Coefficient')

# Calculate summary statistics
mean_estimated_drift <- mean(estimated_drift)
stddev_estimated_drift <- sd(estimated_drift)

# Print the results
cat("True Drift (Actual):", r, "\n")
cat("Mean Estimated Drift:", mean_estimated_drift, "\n")
cat("Standard Deviation of Estimated Drift:", stddev_estimated_drift, "\n")
