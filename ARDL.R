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

###Shows p=1,d=2 and q=0 but d=2 is still not stationary
fit=Arima(cons,order=c(0,2,1))
fit1=Arima(gcf,order=c(0,2,1))
acf(fit$residuals)           #no correlation
qqnorm(fit$residuals)
qqline(fit$residuals)
adf.test(fit$residuals) ####Residuals are stationary
adf.test(fit1$residuals) 
adf.test(fit2$residuals) 
shapiro.test(fit$residuals)  ####Residuals show normality
pred=predict(fit,n.ahead=5)
pred1=predict(fit1,n.ahead=5)

pred00=c(pred$pred)
pred11=c(pred1$pred)

future_value=forecast(fit,h=5,level=c(99.5));future_value
plot(future_value)


lcons=log(pred00)
lgcf=log(pred11)

##############ARDL############
library(readxl)
library(dynamac)
library(forecast)
library(tidyverse)
library(tseries)
library(urca)
library(TSstudio)
library(vars)
library(dLagM)
##STEPS  FOR ARDL MODEL

##1)Check the stationarity of variable(Variables should be stationary at I(0) and I(1)
##2)select the optimum lag
##3)Apply ARDL model

###1)###CONSUMPTION
data=read.csv("C:\\Ak\\stats\\Msc project\\consumption.csv")
d=data[1:60,2]
cons=ts(d)
plot(cons)
diffcons=diff(log(cons),1)
plot(diffcons)
adf.test(diffcons)
####The order of integration is 1

####GCF####
data2<- read.csv("C:\\Ak\\stats\\Msc project\\gcp.csv")
d2=data2[1:60,2]
gcp=ts(d2)
plot(gcp)
diffgcp=diff(log(gcp),1)
plot(diffgcp)
adf.test(diffgcp)
####The order of integration is 1


####GDP###
data3=read.csv("C:\\Ak\\stats\\Msc project\\gdp time.csv")
d3=data3[1:60,1]
gdp=ts(d3)
plot(gdp)
diffgdp=diff(log(gdp),1)
plot(diffgdp)
adf.test(diffgdp)
####The order of integration is 1


logcons=log(cons)
loggcp=log(gcp)
loggdp=log(gdp)


####Verifying the assumptions
#The dependent variable (loggdp) is I(1)
#All the independent variables are either I(0) or I(1)


#2) Optimal Lag selection
VARselect(loggdp)
##AIC(n)  HQ(n)  SC(n) FPE(n) 
##10       7       7     10

###Optimum lag=10

VARselect(loggcp)
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#1      1      1      1 

###Optimum lag=1

VARselect(logcons)
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#  5      1      1        5

###Optimum lag=5

VARselect(data.frame(logcons,loggcp))
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#1      1        1        1 

###Optimum lag=1

###Optimum lag for dependent variable gdp=10
###Optimum lag for combined independent variable consumption and gcp =1



#3) Creating the model
ardldata=data.frame(loggdp,logcons,loggcp)
model1=ardlDlm(formula=loggdp~logcons+loggcp,data=ardldata,p=1,q=10)
summary(model1)
####R^2=0.9993
####Removing the insignificant coefficients
remove=list(p=list(logcons=c(1),loggcp=c(1)),q=(loggdp=c(1,2,3,4,5,6,7,8,9,10)))
model2=ardlDlm(formula=loggdp~logcons+loggcp,data=ardldata,p=1,q=10,remove=remove)
k=summary(model2)
shapiro.test(k$residuals) ## residuals follow normaltiy
GoF(model2)
####R^2=0.9984
###Model 2 drastically reduces the complexity of the model but does not reduce the R^2 as much.





####Lag selection criteria
lagselect=VARselect(ardldata);lagselect
#####lagselect=6-1=5#####

###Johansen Testing(Trace)
ctest1t=ca.jo(ardldata,type="trace",ecdet="const",K=5)
summary(ctest1t)
##There is cointegration


ctest1e=ca.jo(ardldata,type="eigen",ecdet="const",K=5)
summary(ctest1e)

X=c(logcons[56:60],loggcp[56:60])
X=c(lcons,lgcf)
a=forecast(model2,h=5,x=matrix(X,nrow=2,byrow=TRUE),interval=TRUE,level=0.95)
f1=a$forecasts[[2]];f1
fgdp=exp(f1);fgdp
gdpp=matrix(c(gdp))
plot( 0, type = "b", xlim = c(6.37e+12,2.45e+14),
      ylim = c(0,70) )
lines(gdpp,col="red")
