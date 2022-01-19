####### Simulating a standard VAR #######
# We look to simulate a VAR(2) with k = 4 equations
rm(list=ls())
library(yuima)
library(urca)
library(vars)
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
Bmat <- matrix(c(gamma, delta, 0, 0, delta, gamma, 0, 0, 0, 0, gamma, 0, 0, 0, 0, gamma), k) # Gamma matrix
A <- A.1 + A.2 + Bmat

ols <- function(Y,X){ # building OLS function
  b<- solve(crossprod(X), crossprod(X,Y)) # coefficient estimates
  y.hat <- X%*%b # fitted values
  out <- list(coef.estimates = b, fitted.values = y.hat)
  return(out)
}
# first value spits out the coefficient estimate, the second value spits out 
ols.out <- ols(Y,X)
beta.hat <- ols.out[[1]]

# Number of simulations
nr.sim <- 1000
# Initialize a vector of 0s to store rejections
reject.0 <- rep(0, times = nr.sim)
reject.1 <- rep(0, times = nr.sim)
names <- c("V1", "V2", "V3", "V4") # Rename variables

###### Start the Simulation ######
Q <- matrix(data = NA,nrow= nr.sim, ncol = 4)
for (j in 1:nr.sim){
  ## Step 1: Simulate ##
  # Generate sample from VAR
  series <- matrix(0, k, t + 2*p) # Raw series with zeros
  for (i in (p + 1):(t + 2*p)){ # Generate series with e ~ N(0,1)
    series[, i] <- A%*%series[, i-1] - Bmat%*%series[, i-2] + rnorm(k, 0, 1)
  }
  X <- t(series)
  colnames(X) <- names
  
  ##Step 2: Apply Trace test ##
  ca <- ca.jo(X, type = "trace", K = 2, ecdet = "none")
  S <- summary(ca)
  teststats <- rev(S@teststat)
  Q[j,] <- teststats
  ## Step 3: Evaluate ##
  # Check if null hypothesis is rejected
  if (teststats[1] > 48.28) {reject.0[j] <- 1}
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

VARnew <- VAR(X, p = 2, type = "const")
res.VARnew <- residuals(VARnew)
mean.res <- cbind(mean(res.VARnew[,1]), mean(res.VARnew[,2]), mean(res.VARnew[,3]), mean(res.VARnew[,4])) 
mean.res1 <- mean(res.VARnew[,1]); mean.res2 <- mean(res.VARnew[,2]); mean.res3 <- mean(res.VARnew[,3]); mean.res4 <- mean(res.VARnew[,4])
res.cen <- cbind(res.VARnew[,1] - mean.res1, res.VARnew[,2] - mean.res2, res.VARnew[,3] - mean.res3, res.VARnew[,4] - mean.res4) 

##################### THE BOOTSTRAP IN R #####################
# First draw indices of the bootstrap sample: draw n times with replacement
n = 54
J <- ceiling(runif(n, min = 0, max = n))
# we do B bootstrap replications and store the quantities in a vector
B = 199
#reject.star.0 <- rep(0, times = B)
#reject.star.1 <- rep(0, times = B)
Q.star1 <- matrix(data = NA,nrow= B, ncol = 4)   
#Q.star <- rep(NA, times = B);
for (b in 1:B) {
  J <- sample.int(n, size = n, replace = TRUE) # Draw J
  X.star <- X[J,]
  colnames(X.star) <- names
  ca.star <- ca.jo(X.star, type = "trace", K = 2, ecdet = "none")
  S.star <- summary(ca.star)
  teststats.star <- rev(S.star@teststat) #stored as teststat
  Q.star1[b,] <- teststats.star
}
cv.star1 <- quantile(Q.star1[,1], probs=0.95)
cv.star2 <- quantile(Q.star1[,2], probs=0.95)
#if (teststats.star[1] > 48.28) {reject.star.0[b] <- 1}
#if (teststats.star[2] > 31.52) {reject.star.1[b] <- 1}



######### Testing for the Trace test #########
set.seed(123)
nr.sim <- 500; B <- 199;
n <- t + 4;
reject.star.0 <- rep(0, times = nr.sim)
reject.star.1 <- rep(0, times = nr.sim)
X.df <- data.frame(X)
names <- c("V1", "V2", "V3", "V4") # Rename variables
Q.star1 <- matrix(data = NA,nrow= B, ncol = 4) 
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
    Q.star1[b,] <- teststats.star
  }
  cv.star1 <- quantile(Q.star1[,1], probs=0.95)
  cv.star2 <- quantile(Q.star1[,2], probs=0.95)
  
  if (teststats[1] > cv.star1) {reject.star.0[b] <- 1}
  if (teststats[2] > cv.star2) {reject.star.1[b] <- 1}
}

## Step 4: Summarize ##
ERF.0 <- mean(reject.star.0)
ERF.1 <- mean(reject.star.1)
print(paste("Rejection occurred in ", 100 *ERF.0, "% of the cases."))
print(paste("Rejection occurred in ", 100 *ERF.1, "% of the cases."))  


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