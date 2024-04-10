

## Problem: Spurious regression using Monte Carlo Experiment (Q1 ~ Q7) =====================================
# Initialize the seed, helpful for replication
rm(list = ls()); set.seed(1000)

# Q1 =========================================================================================================
T = 100
e = rnorm(T)
y = matrix(c(rep(0,1*T)), nrow = T, ncol = 1)
y[1] = e[1]
for(i in 2:T) {
  y[i] = y[i-1] + e[i]
}
dev.new(noRStudioGD = TRUE)
plot(y, type = 'l')
title('A Path for Variable Y')

# Q2 =========================================================================================================
a = rnorm(T)
x = matrix(c(rep(0,1*T)), nrow = T, ncol = 1)
x[1] = e[1]
for(i in 2:T) {
  x[i]=x[i-1]+a[i]
}
dev.new(noRStudioGD = TRUE)
plot(x,type='l')
title('A Path for Variable X')
graphics.off()

# Q3 =========================================================================================================
summary(lm(y~x))

# Q4 =========================================================================================================
summary(lm(y~x))$r.squared

# Q5 & Q6 ====================================================================================================
set.seed(1000)
nobs  = 100  # number of observation, change the number to 50 and 200 for Question 6
niter = 1000 # number of iteration
phi_y = 1    # coefficient of y
phi_x = 1    # coefficient of x

## create matrices to store relevant info
beta    = matrix(c(rep(0,2*niter)), nrow=2, ncol=niter) # creating matrix for coefficients
tstat   = matrix(c(rep(0,2*niter)), nrow=2, ncol=niter) # creating matrix for t-statistics
rsquare = matrix(c(rep(0,1*niter))) # creating r square matrix

for(i in 1:niter) {
  ## creates the data (points i and ii)
  e = matrix(c(rnorm(nobs))) # data generating e
  a = matrix(c(rnorm(nobs))) # data generating a     
  y = matrix(c(rnorm(nobs)))*0
  x = matrix(c(rnorm(nobs)))*0
  y[1,1] = e[1,1] # starting point for y
  x[1,1] = a[1,1] # starting point for x
  
  for(j in 2:nobs) {
    y[j,1] = phi_y*y[j-1,1] + e[j,1]
    x[j,1] = phi_x*x[j-1,1] + a[j,1]
  }
  
  ## runs a regression on a created data and stores the outcomes
  lmcoef = matrix(coef(summary(lm(y~x))), ncol=4, nrow=2)
  beta[1,i]  = lmcoef[1,1]
  beta[2,i]  = lmcoef[2,1]
  tstat[1,i] = lmcoef[1,3]
  tstat[2,i] = lmcoef[2,3]
  rsquare[i,1] = summary(lm(y~x))$r.squared
}

## construct the histograms for R^2 and t-stats
dev.new(noRStudioGD = TRUE)
par(mfrow=c(1,2))
hist(rsquare, main="R-square distribution", xlab="R-square", ylab="frequency")
hist(tstat[2,], main="t-statistic distribution", xlab="t-value", ylab="frequency")

## get the 5, 50 and 95th percentiles
quantile(tstat[2,], c(.05, .50, .95)) 
quantile(rsquare, c(.05, .50, .95)) 

## calculate the rejection rate with 1.96 critical value
rej = abs(tstat[2,])>1.96
fracofrej = sum(rej)/niter
fracofrej

## In the benchmark case, when T = 100, the rejection given the seed of this code is 0.775,
## as T increases the rejection frequency increases.

## The quantiles of the R^2 do not seem to change as the sample size changes. However the
## distribution of the t-statistic becomes more dispersed. In the limit as T grows large, the
## fraction of the t-statistics that exceed 1.96 in absolute values seems to approach 1.0.

# Q7 =========================================================================================================
rm(list = ls()); set.seed(1000)
nobs  = 100;  # number of observation, change the number to 50 and 200 
niter = 1000; # number of iteration

## create matrices to store relevant info
beta    = matrix(c(rep(0,2*niter)), nrow=2, ncol=niter) # creating matrix for coefficients
tstat   = matrix(c(rep(0,2*niter)), nrow=2, ncol=niter) # creating matrix for t-statistics
rsquare = matrix(c(rep(0,1*niter)));  # creating r square matrix

for(i in 1:niter) {
  ## creates the data (points i and ii)
  e = matrix(c(rnorm(nobs))) # data generating e
  a = matrix(c(rnorm(nobs))) # data generating a     
  y = matrix(c(rnorm(nobs)))*0
  x = matrix(c(rnorm(nobs)))*0
  y[1,1] = e[1,1] # starting point for y
  x[1,1] = a[1,1] # starting point for x
  
  for(j in 2:nobs) {
    y[j,1] = e[j,1]
    x[j,1] = a[j,1]
  }
  
  ## runs a regression on a created data and stores the outcomes
  lmcoef = matrix(coef(summary(lm(y~x))), ncol=4, nrow=2)
  beta[1,i] = lmcoef[1,1]
  beta[2,i] = lmcoef[2,1]
  tstat[1,i] = lmcoef[1,3]
  tstat[2,i] = lmcoef[2,3]
  rsquare[i,1] = summary(lm(y~x))$r.squared
}

## construct the histograms for R^2 and t-stats
dev.new(noRStudioGD = TRUE)
par(mfrow=c(1,2))
hist(rsquare, main="R-square distribution", xlab="R-square", ylab="frequency")
hist(tstat[2,], main="t-statistic distribution", xlab="t-value", ylab="frequency")

## get the 5, 50 and 95th percentiles
quantile(tstat[2,], c(.05, .50, .95)) 
quantile(rsquare, c(.05, .50, .95)) 

#### calculate the rejection rate with 1.96 critical value
rej = abs(tstat[2,])>1.96
fracofrej = sum(rej)/niter
fracofrej

# The rejection rate with 1.96 critical value is 0.043 when T=100.
# Change the sample size with T=50 (T=200) and repeat. 
# The rejection rate is 0.058 when T = 50, and is 0.066 when T = 200. 
# If you run it for larger samples and more iterations you will get closer to 0.05. 
# The behavior of R-square does not change, so the issue is not with the point estimates, 
# but rather with the t-stats and its asymptotic distribution.
