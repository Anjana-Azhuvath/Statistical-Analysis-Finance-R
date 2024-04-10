

library("forecast")
library("ggplot2")
library("gridExtra")
#AR Coeffs
phi1=1.5
phi2=-0.7
#MA Coeffs
theta=0.8
#Sample Size
n=230

#Simulating an AR(2,1) process
m_4a=arima.sim(list(ar=c(1.5, -0.7),ma=c(theta)),n=230)
#Dropping last 30 observations
data=m_4a[1:200]

#Fitted model
arma21=arima(data,order=c(2,0,1))
arma211=arima(data,order=c(2,1,1))

#Extracting Residuals
arma21_resid=arma21$residuals
arma211_resid=arma211$residuals

#arma21 forecast
arma21_forecast=forecast(arma21,h=20,level=0.99,bootstrap=FALSE)
arma_211_forecast=forecast(arma211,h=20,level=0.99,bootstrap=FALSE)

#Compare forecast with observation

autoplot(arma21_forecast)
autoplot(arma_211_forecast)









