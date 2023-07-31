library(readxl)
Project1 <- read_excel("G:\\M.Sc\\M.Sc Project\\Project1.xlsx")
View(Project1)
d=Project1
library("tseries")
library("forecast")
library("MASS")
t=ts(d);t
x=t[,1]
y=t[,2]
plot.ts(x,y)
acf(y,type="correlation")
acf(y,type="partial")
##ACF is gradually decreasing and PACF cuts at lag 1
adf.test(y,alternative="stationary")
##The process is not stationary
##ndiffs(y,alpha=0.05,test="adf")
##diff_d=diff(y,difference =2)
##plot.ts(diff_d)
##adf.test(diff_d,alternative="stationary")
##The series is stationary.
auto.arima(y,trace=TRUE)
a=arima(y,order=c(0,2,1));a
acf(a$residuals)           
Box.test(a$residuals,type="Ljung-Box") #Residuals show no significant correlation
adf.test(a$residuals)      ####Residuals are stationary
qqnorm(a$residuals)
qqline(a$residuals)
shapiro.test(a$residuals)  ####Residuals show normality
predict(a,n.ahead=5)
futureval=forecast(a,h=10,level=c(99.5));futureval
plot(futureval)


