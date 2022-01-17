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
delta <- 0
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
names <- c("V1", "V2", "V3", "V4") # Rename variables
transseries <- t(series)
ts.plot(transseries)


## Cointegration test ##
#logseries <- (series
colnames(transseries) <- names

ca <- ca.jo(transseries, type = "trace", K = 2, ecdet = "none")
S <- summary(ca)
str(S)
cv <- c(48.28, 31.52, 17.95, 8.18)
teststats <- rev(S@teststat)
c.rank <-0
for(i in 1:4){
  if(teststats[i]< cv[i]){
    return(c.rank <- i-1) 
  }
}

# Number of simulations
nr.sim <- 10
# Initialize a vector of 0s to store rejections
reject.n <- rep(0, times = nr.sim)
# Initialize a vector of 0s to store rejections
reject.t <- rep(0, times = nr.sim)

###### Start the Simulation ######

for (j in 1:nr.sim){
  ## Step 1: Simulate ##
  # Generate sample from VAR
  series <- matrix(0, k, t + 2*p) # Raw series with zeros
  for (i in (p + 1):(t + 2*p)){ # Generate series with e ~ N(0,1)
    series[, i] <- A%*%series[, i-1] - B%*%series[, i-2] + rnorm(k, 0, 1)
  }
  
  seriests <- ts(t(series[, -(1:p)])) # Convert to time series format
  names <- c("V1", "V2", "V3", "V4") # Rename variables
  transseries <- t(series)
  colnames(transseries) <- names
  
  ##Step 2: Apply Trace test ##
  ca <- ca.jo(transseries, type = "trace", K = 2, ecdet = "none")
  S <- summary(ca)
  #str(S)
  cv <- c(48.28, 31.52, 17.95, 8.18)
  teststats <- rev(S@teststat)
  c.rank <-0
  for(i in 1:4){
    if(teststats[i]< cv[i]){
      return(c.rank[j] <- i-1) 
    }
  }
  ## Step 3: Evaluate ##
  # Check if null hypothesis is rejected
  #if (Q.n > cv.n) {reject.n[j] <- 1}
  #if (Q.n > cv.t) {reject.t[j] <- 1}
}


########## Trace Test Function ########## 

tracetest <- function(Y,X){
  abs(solve())
  z <- max(eigen(Y)$values,eigen(X)$values)
  Q <- -t*sum(log(1-z),)
}


