####### Simulating a standard VAR #######
# We look to simulate a VAR(2) with k = 4 equations
rm(list=ls())
library(urca)
# Pulled from r-econometrics

set.seed(123) # Reset random number generator for reasons of reproducability

# Generate sample
t <- 50 # Number of time series observations
k <- 4 # Number of endogenous variables
p <- 2 # Number of lags

# Generate coefficient matrices
a <- -0.4
gamma <- 0.8
delta <- 0
r.delta <- sample(delta, 1) # Create list with random delta's 
alpha <- t(t(c(a,0,0,0)))
beta <- t(t(c(1,0,0,0)))
A.1 <- alpha %*% t(beta) # Alpha matrix
A.2 <- diag(x = 1, k) # 4x4 identity matrix
B <- matrix(c(gamma, delta, 0, 0, delta, gamma, 0, 0, 0, 0, gamma, 0, 0, 0, 0, gamma), k) # Gamma matrix
A <- A.1 + A.2 + B

# Number of simulations
nr.sim <- 10000
# Initialize a vector of 0s to store rejections
reject.0 <- rep(0, times = nr.sim)
reject.1 <- rep(0, times = nr.sim)

###### Start the Simulation ######
cv <- c(48.28, 31.52, 17.95, 8.18)

for (j in 1:nr.sim){
  ## Step 1: Simulate ##
  # Generate sample from VAR
  series <- matrix(0, k, t + 2*p) # Raw series with zeros
  for (i in (p + 1):(t + 2*p)){ # Generate series with e ~ N(0,1)
    series[, i] <- A%*%series[, i-1] - B%*%series[, i-2] + rnorm(k, 0, 1)
  }
  names <- c("V1", "V2", "V3", "V4") # Rename variables
  transseries <- t(series)
  colnames(transseries) <- names
  
  ##Step 2: Apply Trace test ##
  ca <- ca.jo(transseries, type = "trace", K = 2, ecdet = "none")
  S <- summary(ca)
  teststats <- rev(S@teststat)

  ## Step 3: Evaluate ##
  # Check if null hypothesis is rejected
  if (teststats[1] > 48.25) {reject.0[j] <- 1}
  if (teststats[2] > 31.52) {reject.1[j] <- 1}
  }

## Step 4: Summarize ##
# Empirical rejection frequency of rank = 0
ERF.0 <- mean(reject.0)
# Empirical rejection frequency of rank = 1
ERF.1 <- mean(reject.1)
# give the output on the screen
  print(paste("Chance to reject 0: ", ERF.0))
  print(paste("Chance to reject 1: ", ERF.1))

  
  
### NOTES ###
  
  # Check eigenvalues
  #eigen(A.1)
  #eigen(A.2)
  #eigen(A) # Unstable eigenvalues
  #eigen(B) # 1 eigenvalue is equal to 1
  
  
  # Generate series
  #series <- matrix(0, k, t + 2*p) # Raw series with zeros
  #for (i in (p + 1):(t + 2*p)){ # Generate series with e ~ N(0,1)
  #  series[, i] <- A%*%series[, i-1] - B%*%series[, i-2] + rnorm(k, 0, 1)
  #}
  #seriests <- ts(t(series[, -(1:p)])) # Convert to time series format
  #ts.plot(seriests)
  #transseries <- t(series)
  #names <- c("V1", "V2", "V3", "V4") # Rename variables
  #colnames(transseries) <- names
  
  #ca <- ca.jo(transseries, type = "trace", K = 2, ecdet = "none")
  #S <- summary(ca)
  #cv <- c(48.28, 31.52, 17.95, 8.18)
  #teststats <- rev(S@teststat)
  #for(i in 1:4){
  #  if(teststats[i]< cv[i]){
  #    return(c.rank <- i-1) 
  #  }
  #}