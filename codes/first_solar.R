library("fGarch")
library("tigerstats")
library("ggplot2")
library("TTR")
library("gridExtra")
library("quantmod")

#Loading data
fslr=getSymbols("FSLR",src="yahoo", auto.assign=FALSE)
#Extracting closing price
fslr_price=data.frame(date = index(fslr), value = coredata(fslr$FSLR.Close))
#Calculating log returns
log_rtn=diff(log(fslr_price$FSLR.Close))
returns = c()
for(i in 2:nrow(fslr_price)){
  returns[i]=((fslr_price$FSLR.Close[i]-fslr_price$FSLR.Close[i-1])-1)  
}
returns=na.omit(returns)
#Creating a dataframe
firstsolar_ret=as.data.frame(cbind(returns, log_rtn))
#Running a t test
pairedtest=t.test(log_rtn, returns, paired = TRUE)
pairedtest
#Calculating mean and standard deviation
meanLogReturns= mean(log_rtn)
meanNormalReturns= mean(returns)
sdLogReturns=sd(log_rtn)
sdNormalReturns=sd(returns)
iid=table(meanLogReturns,meanNormalReturns, sdLogReturns, sdNormalReturns)

#H0: The means of the log returns and normal returns are equal.
#H1: The means of the log returns and normal returns are not equal.

#Since the p value is not greater than 0.05 at the 5% level of significance, we reject the null hypothesis.
#Therefore, the means from the log returns and normal returns are not equal. The statistical assumption is that the data has to be iid.
#For iid the sample standard deviation will be close to one and sample mean will be close to zero. In case of the log returns, 
#the mean is close to zero but the std deviation is not close to one. In case of normal returns, both conditions are not satistified. 
#The assumptions are not met to run a t-test. Running a t-test on non-iid data would lead to underestimation and a large number of rejections.
