####### Simulating a standard VAR #######
# We look to simulate a VAR(2) with k = 4 equations
rm(list=ls())

# Pulled from r-econometrics

set.seed(123) # Reset random number generator for reasons of reproducability

# Generate sample
t <- 50 # Number of time series observations
k <- 4 # Number of endogenous variables
p <- 2 # Number of lags

# Generate coefficient matrices
a <- -0.4
gamma <- 0.8
delta <- 0.1
r.delta <- sample(delta, 1) # Create list with random delta's 
alpha <- t(t(c(a,0,0,0)))
beta <- t(t(c(1,0,0,0)))

A.1 <- alpha %*% t(beta) # Alpha matrix
A.2 <- diag(x = 1, k) # 4x4 identity matrix
B <- matrix(c(gamma, delta, 0, 0, delta, gamma, 0, 0, 0, 0, gamma, 0, 0, 0, 0, gamma), k) # Gamma matrix
A <- A.1 + A.2 + B

# Check eigenvalues
#eigen(A.1)
#eigen(A.2)
#eigen(A) # Unstable eigenvalues
#eigen(B) # 1 eigenvalue is equal to 1


# Generate series
series <- matrix(0, k, t + 2*p) # Raw series with zeros
for (i in (p + 1):(t + 2*p)){ # Generate series with e ~ N(0,1)
  series[, i] <- A%*%series[, i-1] - B%*%series[, i-2] + rnorm(k, 0, 1)
}

seriests <- ts(t(series[, -(1:p)])) # Convert to time series format
ts.plot(seriests)
transseries <- t(series)
names <- c("V1", "V2", "V3", "V4") # Rename variables
colnames(transseries) <- names

## Cointegration test using ca.jo##
ca <- ca.jo(transseries, type = "trace", K = 2, ecdet = "none")
S = summary(ca)
str(S)
teststat <- rev(S@teststat)
for(i in 1:4){
  if()
}

## Cointegration ourselves ##
tracetest <- function(y,x){-t * sum }

#for(   ){
#  r = 0 if test < crit value return r = 0
#  else test r + 1 
#}


# Number of simulations
nr.sim <- 1000
# set the true value of the cointegration rank

# Initialize a vector of 0s to store rejections
reject.n <- rep(0, times = nr.sim)
# Initialize a vector of 0s to store rejections
reject.t <- rep(0, times = nr.sim)

###### Start the Simulation ######

for (i in 1:nr.sim){
  ## Step 1: Simulate ##
  # Generate sample from VAR
  seriessim <- matrix(0, k, t + 2*p) # Raw series with zeros
  for (i in (p + 1):(t + 2*p)){ # Generate series with e ~ N(0,1)
    seriessim[, i] <- A%*%seriessim[, i-1] - B%*%seriessim[, i-2] + rnorm(k, 0, 1)
  }
  ##Step 2: Apply Trace test ##
 
  
  ## Step 3: Evaluate ##
  # Check if null hypothesis is rejected
  if (Q.n > cv.n) {reject.n[i] <- 1}
  if (Q.n > cv.t) {reject.t[i] <- 1}
}
mean(series)


