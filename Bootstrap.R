### Bootstrap ####
rm(list=ls())
library(urca)
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
Bmat <- matrix(c(gamma, delta, 0, 0, delta, gamma, 0, 0, 0, 0, gamma, 0, 0, 0, 0, gamma), k) # Gamma matrix
A <- A.1 + A.2 + Bmat

######### Testing for the Mean #########
set.seed(123)
nr.sim <- 1000; B <- 399;
n <- t + 4;
reject.star.0 <- rep(0, times = nr.sim)
reject.star.1 <- rep(0, times = nr.sim)
X.df <- data.frame(X)
names <- c("V1", "V2", "V3", "V4") # Rename variables

for (i in 1:nr.sim){
  ## Step 1: Simulate ##
  series <- matrix(0, k, t + 2*p) # Raw series with zeros
  for (i in (p + 1):(t + 2*p)){ # Generate series with e ~ N(0,1)
    series[, i] <- A%*%series[, i-1] - Bmat%*%series[, i-2] + rnorm(k, 0, 1)
  }
  seriests <- ts(t(series[, -(1:p)])) # Convert to time series format
  
  X <- t(series)
  colnames(X) <- names
  ## Step 2: Apply ##
  ca <- ca.jo(X, type = "trace", K = 2, ecdet = "none")
  S <- summary(ca)
  teststats <- rev(S@teststat)
  for (b in 1:B){ # nested bootstrap step
    J <- sample.int(n, size = n, replace = TRUE) # Draw J
    X.star1 <- X.df$V1[J] # put elements of X corresponding to the drawn indices in a vector X*
    X.star2 <- X.df$V2[J]
    X.star3 <- X.df$V3[J]
    X.star4 <- X.df$V4[J]
    X.star <- cbind(X.star1,X.star2,X.star3,X.star4)
    colnames(X.star) <- names
    ca.star <- ca.jo(X.star, type = "trace", K = 2, ecdet = "none")
    S.star <- summary(ca.star)
    teststats.star <- rev(S.star@teststat) #stored as teststat
    if (teststats.star[1] > 48.28) {reject.star.0[b] <- 1}
    if (teststats.star[2] > 31.52) {reject.star.1[b] <- 1}
  }
}
## Step 4: Summarize ##
ERF.0 <- mean(reject.star.0)
ERF.1 <- mean(reject.star.1)
print(paste("Rejection occurred in ", 100 *ERF.0, "% of the cases."))
print(paste("Rejection occurred in ", 100 *ERF.1, "% of the cases.")) 