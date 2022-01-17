# our R script goes here

# simulate a VAR(2) the Smeekes way #
phi.1 <- matrix(c(0.5,0.1,0.2,0.4), nrow = 4) # copied the numbers from slides
phi.2 <- matrix(c(0.5,0.1,0.2,0.4), nrow = 4)
eps <- matrix(rnorm(400), ncol = 4) # given n is 100
X <- matrix(nrow = 100, ncol = 4)
X[1,] <- eps[1,] # first starting point for "for" loop
for (t in 2:100) {
  X[t,] <- X[t-1,]%*%t(phi.1) + X[t-2,]%*%phi.2 + eps[t-1,]
  #  X[t-1,]%*%t(phi.1) + eps[t,] + X[t-2,]%*%(t-1)(phi) + eps[t-1,]
}
# simulate var(2) source: https://www.r-econometrics.com/timeseries/varintro/ #
# Generate sample
t <- 100 # Number of time series observations
k <- 2 # Number of endogenous variables
p <- 2 # Number of lags

# Generate coefficient matrices
A.1 <- matrix(c(-.3, .6, -.4, .5), k) # Coefficient matrix of lag 1
A.2 <- matrix(c(-.1, -.2, .1, .05), k) # Coefficient matrix of lag 2
A <- cbind(A.1, A.2) # Companion form of the coefficient matrices

# Generate series
series <- matrix(0, k, t + 2*p) # Raw series with zeros
for (i in (p + 1):(t + k*p)){ # Generate series with e ~ N(0,1)
  series[, i] <- A.1%*%series[, i-1] + A.2%*%series[, i-2] + rnorm(k, 0, 1)
}
