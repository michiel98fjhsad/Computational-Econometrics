####### Simulating a standard VAR #######
# We look to simulate a VAR(2) with k = 4 equations
rm(list=ls())

phi <- matrix(c(0.5,0.1,0.2,0.4), nrow = 2)
eps <- matrix(rnorm(200), ncol = 2) # given n is 100
X <- matrix(nrow = 100, ncol = 2)
X[1,] <- eps[1,] # first starting point for "for" loop
for (t in 2:100) {
  X[t,] <- X[t-1,]%*%t(phi) + eps[t,]
}


# Pulled from r-econometrics

set.seed(123) # Reset random number generator for reasons of reproducability

# Generate sample
t <- 200 # Number of time series observations
k <- 4 # Number of endogenous variables
p <- 2 # Number of lags

# Generate coefficient matrices
gamma <- 0.8
delta <- 0
alpha <- -0.4
A.1 <- diag(x = alpha, k) # Alpha matrix
A.2 <- diag(x = 1, k) # 4x4 identity matrix
A.3 <- matrix(c(gamma, delta, 0, 0, delta, gamma, 0, 0, 0, 0, gamma, 0, 0, 0, 0, gamma), k) # Gamma matrix
A <- A.1 + A.2 + A.3

# Check eigenvalues
eigen(A.1)
eigen(A.2)
eigen(A.3)
eigen(A)
A <- cbind(A.1, A.2) # Companion form of the coefficient matrices

test <- rnorm(k,0,1)

# Generate series
series <- matrix(0, k, t + 2*p) # Raw series with zeros
for (i in (p + 1):(t + 2*p)){ # Generate series with e ~ N(0,1)
  series[, i] <- A.1%*%series[, i-1] + A.2%*%series[, i-2] + rnorm(k, 0, 1)
}

series <- ts(t(series[, -(1:p)])) # Convert to time series format
names <- c("V1", "V2") # Rename variables

plot.ts(series) # Plot the series


