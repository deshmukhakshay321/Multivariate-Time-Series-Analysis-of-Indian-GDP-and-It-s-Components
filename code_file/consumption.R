data=read.csv("G:\\M.Sc\\M.Sc Project\\consumption.csv")
View(data)
d=data[1:60,2] ####Excluding 2020 and 2021 
library("readxl")
library("tseries")
library("forecast")
library("MASS")
t=ts(d)
plot.ts(t)
acf(t,type="correlation")
acf(t,type="partial")
#####Looks like parameter p=1 and q=0 from the ACF showing geometric decay and pacf cutting at lag 1

###Ho=The process is not stationary
###H1=The process is stationary
adf.test(t,alternative="stationary")
####Since p-value is 0.99>p-value=0.05 we accept H0 that the process is not stationary
###Ho=There is no autocorrelation for pi=0
###H1=There is autocorrelation
#Box.test(t,type="Ljung-Box") ##P-value=2.718e-14 reject Ho
#ndiffs(t,alpha=0.05,test="adf")

###Differencing by lags 2
###differenced=diff(t,lag=2)
###plot.ts(differenced)
adf.test(differenced,alternative="stationary")

m1=auto.arima(t,trace=TRUE);m1
###Shows p=1,d=2 and q=0 
fit=Arima(t,order=c(1,2,0))
acf(fit$residuals) 
Box.test(fit$residuals,type="Ljung-Box")#Residuals show no significant correlation
adf.test(fit$residuals)      ####Residuals are stationary
qqnorm(fit$residuals)
qqline(fit$residuals)
shapiro.test(fit$residuals)  ####Residuals show normality
predict(fit,n.ahead=5)
futureval=forecast(fit,h=10,level=c(99.5));futureval
plot(futureval)
