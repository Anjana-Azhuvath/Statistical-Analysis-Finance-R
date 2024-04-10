library("xts")
setwd("/Users/anjanaraja/Documents/midtermexam")

nflx = read.csv("NFLX.csv") 
Date=as.Date(nflx$Date,'%Y-%m-%d')
ret=diff(log(nflx$Close))
data=nflx[-1,]
data$log.Return=ret
data$Close=as.numeric(data$Close)
data$log.Return=as.numeric(data$log.Return)
data$Date=as.Date(data$Date,'%Y-%m-%d')
#########plotting data
library("ggplot2")
library("gridExtra")
closing_plt=ggplot(data)+
  geom_line(aes(x=Date , y=Close), color="red")+
  theme_minimal()+
  labs(title = "Time Series of Netflix Closing Prices",
       x = "Date",
       y = "Closing Price")
ret_plt=ggplot(data)+
  geom_line(aes(x=Date , y=log.Return), color="blue")+
  theme_minimal()+
  labs(title = "Time Series of Netflix Log Returns",
       x = "Date",
       y = "Closing Price")

grid.arrange(closing_plt, ret_plt,nrow=2)

##############identifying outlier
which.min(data$Close)

###############Shapiro Test
shapiro_test=shapiro.test(data$log.Return)
shapiro_test

###############DGP test
library(testcorr)
dgp_test=ac.test(data$log.Return,max.lag = 10,plot = TRUE)
dgp_test$pvttilde
dgp_test

#############MLE- T Distribution
library("fGarch")
df=data[-27,]
x=diff(log(df$Close))
loglik.t = function(beta){
  
  center = beta[1]
  scale = beta[2]
  nu = beta[3]
  n = length(x)
  
  # nlike = -sum(log(dt((x-mean)/scale,nu)*(1/scale)))    
  # This is the same as 
  
  nlike = -sum(dt((x-center)/scale,df=nu,log=TRUE) + log(1/scale))
  
  # Note, that setting log = TRUE means the log density is evaluated
  
  return(nlike)
  
}

ini = c(mean(x),sd(x),3)

fit.t = optim(ini,loglik.t,hessian=T,method = "L-BFGS-B", lower = c(-1,0.001,1))

fit.t$par


####################### Density of normal T distribution [Not Working]
beta = fit.t$par

den = density(x,bw = "sj", kernel = "gaussian")
xaxis = den$x
y = dt((xaxis-beta[1])/beta[2],beta[3])*(1/beta[2])
par(mfrow = c(1,1))
plot(den,main = "KDE for logdiffclose",col = "blue")
lines(xaxis,y, col = "red")

###################### Skewed T-distribution

x=diff(log(df$Close))

loglik.std.t = function(beta){
  
  center = beta[1]
  std = beta[2]
  nu1 = beta[3]
  xi1 = beta[4]
  
  nlike = -sum(dsstd(x,mean = center, sd = std, nu = nu1, xi = xi1, log=TRUE))
  
  return(nlike)
  
}
ini = c(mean(x),sd(x),nu=3.8,xi=1)
fit.std.t = optim(ini,loglik.std.t,hessian=T,method = "L-BFGS-B", lower = c(-1,0.001,2.1,0.1))
fit.std.t$par
fit.std.t
mle_t=fit.t$value
mle_t
mle_skew=fit.std.t$value
mle_skew
mle_diff=mle_skew-mle_t
mle_diff
llr=2*mle_diff
llr
1-pchisq(2.294793, df=3.8)

#########Confidence Interval
fit.std.beta=fit.std.t$par
fit.std.beta
A=solve(fit.std.t$hessian)
B=diag(A)
std_error=sqrt(B[1])

interval_upper=fit.std.beta[1]+1.96*std_error
interval_upper
interval_lower=fit.std.beta[1]-1.96*std_error
interval_lower
#################
2*(fit.std.t$value) + 2*2
2*(fit.t$value) + 2*3