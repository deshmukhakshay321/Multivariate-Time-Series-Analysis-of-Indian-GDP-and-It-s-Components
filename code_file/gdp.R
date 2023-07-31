book=read.csv("G:/M.Sc/M.Sc Project/gdp time.csv")
library(forecast)
library(tseries)
d2=book[1:60,2]
d1=book[,1]
d=ts(d2)
plot.ts(d)
acf(d,type='correlation')
acf(d,type='partial')
adf.test(d)
model=auto.arima(d,trace=TRUE);model
###Shows p=0,d=2 and q=1
fit=Arima(d,order=c(0,2,1))
acf(fit$residuals)    
Box.test(fit$residuals,type="Ljung-Box")#Residuals show no significant correlation
adf.test(fit$residuals)      ####Residuals are stationary
qqnorm(fit$residuals)
qqline(fit$residuals)
shapiro.test(fit$residuals)  ####Residuals show normality
predict(fit,n.ahead=5)
future_value=forecast(fit,h=10,level=c(99.5));future_value
plot(future_value)

