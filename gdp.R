book=read.csv("C:/Ak/stats/Msc project/projecttt.csv")
library(forecast)
library(tseries)
d2=book[,2]
d1=book[,1:3]
d=ts(d1)
cons=d[,1]
gcf=d[,2]
gdp=d[,3]
plot.ts(d)
acf(d,type='correlation')
acf(d,type='partial')
adf.test(d)
model=auto.arima(cons,trace=TRUE);model
model=auto.arima(gcf,trace=TRUE);model
model=auto.arima(gdp,trace=TRUE);model
###Shows p=1,d=2 and q=0 but d=2 is still not stationary
fit=Arima(cons,order=c(0,2,1))
fit1=Arima(gcf,order=c(0,2,1))
fit2=Arima(gdp,order=c(0,2,1))
acf(fit$residuals)           #no correlation
qqnorm(fit$residuals)
qqline(fit$residuals)
adf.test(fit$residuals) ####Residuals are stationary
adf.test(fit1$residuals) 
adf.test(fit2$residuals) 
shapiro.test(fit$residuals)  ####Residuals show normality
pred=predict(fit,n.ahead=5)
pred1=predict(fit1,n.ahead=5)
pred2=predict(fit2,n.ahead=5)

pred00=c(pred$pred)
pred11=c(pred1$pred)
pred22=c(pred2$pred)
future_value=forecast(fit,h=5,level=c(99.5));future_value
plot(future_value)
