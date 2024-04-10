###################################################################################################################################################
###################################################################################################################################################
                      "REAL ROOTS OF AN AR(2) PROCESS"
###################################################################################################################################################
###################################################################################################################################################
"Construct a stationary AR(2) model whose roots, lamda1 and lamda2 are both real.

    Condition:1<lamda1<lamda2<1.3"

"* Write the expression for this AR(2) model (using both the classical AR(2) regression expression and the backshift notation)."
lambda1=1.1
lambda2=1.2

"The classical regression equaltion will be as follows:"
n1=150
samp=rnorm(n1)
phi1=1/lambda1 +1/lambda2
phi2=-((1/lambda1)*(1/lambda2))
phi1
phi2
lag_ar=log(n1)
  
"* Make a plot of the true ACF, true PACF and the corresponding (including the Rcode that you use)."
library(ggplot2)
library(gridExtra)

real_acf=ARMAacf(ar=c(phi1,phi2), ma=0, lag_ar)
real_pacf=ARMAacf(ar=c(phi1,phi2), ma=0, lag_ar,pacf=TRUE)


###################################################################
############# TRUE ACF PLOTS#####################################
real_acf=as.data.frame(real_acf)
lag=0:lag_ar
real_acf$lag=lag=0:lag_ar
true_acf_plt=ggplot(real_acf,aes(x=lag, y=real_acf))+
  geom_point()+
  geom_line() 
true_acf_plt
###################################################################
############# TRUE PACF PLOTS#####################################
pacf_data=data.frame(lag=1:length(real_pacf), pacf=real_pacf)

true_pacf_plt=ggplot(pacf_data, aes(x = lag, y = pacf))+
  geom_point() + # Add points for each PACF value
  geom_line()  # Connect points with lines
true_pacf_plt

grid.arrange(true_acf_plt, true_pacf_plt, ncol = 2)

"* Make a plot of the roots together with the unit circle."
f <- function(x) exp(2*pi*1i*x)
x <- seq(0, 2*pi, by=0.001)
plot(f(x), type  = "l", xlim = c(-3,3), ylim = c(-3,3),xlab = "real", ylab = "imaginary",asp=1)
ar_2 = c(1,phi1,phi2)
points(polyroot(ar_2),col="blue", pch=15)

"* Simulate a realisation from this model using sample size n = 150"
ar_2 = c(1,phi1,phi2)
real_acf=ARMAacf(ar=c(phi1,phi2), ma=0, lag_ar)
real_acf_sim = arima.sim(list(real_acf),n = n1)
par(mfrow=c(1,2))
acf(real_acf_sim, main = "ACF of Sim")
pacf(real_acf_sim, main = "PACF of Sim")







###################################################################################################################################################
###################################################################################################################################################
                  "Complex ROOTS OF AN AR(2) PROCESS"
###################################################################################################################################################
####################################################################################################
library(stats)

phi1 <- -1.01
phi2 <- 1.01
roots <- polyroot(c(-phi2, phi1, 1))
roots
# Simulate AR(2) process
set.seed(123) # For reproducibility
n <- 100 # Number of observations
c <- 0 # Assuming the constant term is 0 for simplicity

# Simulate white noise
epsilon <- rnorm(n)

# Initialize the time series
Y <- numeric(n)

# Assume initial values
Y[1] <- rnorm(1)
Y[2] <- rnorm(1)

# Generate the AR(2) series
for(t in 3:n){
  Y[t] <- c + phi1 * Y[t-1] + phi2 * Y[t-2] + epsilon[t]
}

# Plot the AR(2) series
plot(Y, type = 'o', col = 'blue', main = 'Simulated AR(2) Model', xlab = 'Time', ylab = 'Value')