library("xts")
library("ggplot2")
library("gridExtra")

setwd("/Users/anjanaraja/Documents/midtermexam")

#loading data
raw=read.csv("TradeDeficit.csv")
raw=as.data.frame(raw)
raw$Date=as.Date(raw$DATE)
raw$Deficit=as.numeric(raw$MTSDS133FMS)   #Convert deficits data to numeric
trade=data.frame(Date=raw$Date, Deficit=raw$Deficit)
class(trade)
lag1=diff(trade$Deficit, lag=1)
lag12=diff(trade$Deficit,lag=12)
#log deficit
lag1 = diff(trade$Deficit,lag = 1)
seasonal_diff= diff(lag1,lag = 12)
first_diff=lag1[-c(1:11)]
Date=trade$Date[-c(1:12)]


deseason= arima(first_diff,order=c(1,0,0),seasonal = list(order = c(1,0,0),period = 12))

data=as.data.frame(cbind(Date,lag12, first_diff))


### plotting the data
actual_plot=ggplot(trade) +
  geom_line(aes(x = Date, y = Deficit), show.legend = TRUE, color="blue")+
  ggtitle("Actual Trade Deficits of US")+theme_bw()+ theme(plot.title = element_text(size = 10))+
  scale_y_continuous(limits = c(-2e+05, 2e+05))

diff12_plot=ggplot(data)+
  geom_line(aes(x = Date, y = lag12), show.legend = TRUE,color="red")+
    ggtitle("Twelveth Lag of Trade Deficits of USA")+theme_bw()+ theme(plot.title = element_text(size = 10))+
  scale_y_continuous(limits = c(-2e+05, 2e+05))

diff1_plot=ggplot(data)+
  geom_line(aes(x = Date, y = first_diff), show.legend = TRUE,color="black")+
  ggtitle("First Lag of Trade Deficits of USA")+theme_bw()+ theme(plot.title = element_text(size = 10))+
  scale_y_continuous(limits = c(-2e+05, 2e+05))

deseason_plot=ggplot(data)+
  geom_line(aes(x = Date, y = zt), show.legend = TRUE,color="green")+
  ggtitle("Deseasonalized Trade Deficits of USA")+theme_bw()+ theme(plot.title = element_text(size = 10))
 

combined=grid.arrange(actual_plot,diff12_plot,diff1_plot,deseason_plot, nrow=3)


########Fitting ARIMA Models to Data

test1 = arima(lag12,order = c(3,0,2))
test1$coef
par(mfrow = c(1,1))
coef = c(1,-0.0771747 ,0.5554320, 0.3705244,0.0999028, -0.6012752,75.2635887)

f <- function(x) exp(2*pi*1i*x)
x <- seq(0, 2*pi, by=0.001)
plot(f(x), type  = "l", xlim = c(-3,3), ylim = c(-3,3),xlab = "real", ylab = "imaginary",asp = 1)
points(polyroot(coef),col="blue", pch=15)













