library("graphics")
library("tseries")
library("stats")
library("forecast")
library("rugarch")
library("parallel")
library("lubridate")
library("ggplot2")
library("tidyverse")
library("dplyr")
library("stargazer")

##loading data
library("summarytools")

##loading data
todo<-read.csv("/Users/Kasutaja/Downloads/NES.csv", header = T)
descr(todo)
tp <- ts(todo$HSBA.L,start = decimal_date(as.Date("2014-01-03")), frequency=252)
plot(tp)
#plot(todo$Adj.Close)

#bdm=ts(read.table("BDM.txt",header=F),frequency = 12,start=1985)
#bdm_train=ts(bdm[1:367],frequency = 12,start=1985)


#getting the returns
rt <- diff(log(todo$HSBA.L)) 
rt
acf(rt, 20)
pacf(rt,20)
#rt
#plot(rt)
#lag.plot(rt,9,do.lines=FALSE)

summary(rt)

##defining the T.S.of the returns (date of start and frequency)
#trt <- ts(rt,start = decimal_date(as.Date("2014-01-03")), frequency=252)
trt <- ts(rt,start = 2014, frequency=252)

trt
plot(trt)
summary(trt)

#trt
#For outliers boxplot
#boxplot(rt,col="purple",outlier="red")

#We can observe that the returns are moving around a constant mean, 
#there is volatility, there are positive and negative jumps 
#lag.plot(trt,10,do.lines=FALSE)

acf(trt, 20)
pacf(trt,20)

#this one describe the T.S. decomposition
plot(estacion <- stl(trt,"per"))

##For modeling the conditional mean, we try with ARMA(0,0), ARMA(1,1) and ARMA(2,2)
#with the best fit from the ARMA(0,0)

# ARMA(2,1)
x.arma21 <- arima(trt, order = c(2, 0, 1))
x.arma21
#x11()
par(mfrow=c(1,2))
acf(resid(x.arma21),main="ACF de los residuales ARMA(2,1)")
pacf(resid(x.arma21),main="PACF de los residuales ARMA(2,1)")

# ARMA(1,1)
x.arma11 <- arima(trt, order = c(1, 0, 1))
x.arma11
#x11()
par(mfrow=c(1,2))
acf(resid(x.arma11),main="ACF de los residuales ARMA(1,1)")
pacf(resid(x.arma11),main="PACF de los residuales ARMA(1,1)")

# ARMA(0,0)
x.arma00 <- arima(trt, order = c(0, 0, 0))
x.arma00
#x11()
par(mfrow=c(1,2))
acf(resid(x.arma00),main="ACF de los residuales ARMA(0,0)")
pacf(resid(x.arma00),main="PACF de los residuales ARMA(0,0)")

# ARMA(2,2)
x.arma22 <- arima(trt, order = c(2, 0, 2))
x.arma22
par(mfrow=c(1,2))
acf(resid(x.arma22),main="ACF de los residuales ARMA(2,2)")
pacf(resid(x.arma22),main="PACF de los residuales ARMA(2,2)")

##We will use the automatic analysis:
#automático
auto.arima(trt,trace=TRUE)

# Comparación del AIC
#AIC(x.ar)
#AIC(x.ma)
AIC(x.arma21)
AIC(x.arma11)
AIC(x.arma00)
AIC(x.arma22)
BIC(x.arma21)
BIC(x.arma11)
BIC(x.arma00)
BIC(x.arma22)

##For modeling the time-varying volatility R the GARCH models, 
#setting the ARMA(0,0), given the previous result and a t-student distribution

#######garch with t-student distribution

sgarch <- ugarchspec(mean.model=list(armaOrder=c(0,0)),
                variance.model=list(model="sGARCH"),
                distribution.model="normal")
sg<- ugarchfit(data=trt,spec=sgarch, out.sample=200)
sg
plot(sg,which="all")
autoplot(sg)


f<- ugarchforecast(fitORspec = sg,n,n.ahead = 252)
plot(f,which="all")
plot(fitted(f))
plot(sigma(f))


f1<- ugarchforecast(sg,n,n.roll = 100)
plot(f1,which="all")



##mejor este ya que todos los parametros son estadisticamente 
#significantes, al aplicar el gjr garch se pierde con valores mayores al 5%


#######GJR - garch with t-student distribution

sjgr <- ugarchspec(mean.model=list(armaOrder=c(0,0)),
                variance.model=list(model="gjrGARCH"),
                distribution.model="sstd")
jgr<- ugarchfit(data=trt,spec=sjgr)
jgr
plot(jgr,which="all")

f<- ugarchforecast(fitORspec = jgr,n,n.ahead = 252)
f1<- ugarchforecast(jgr,n)
plot(f1,which="all")



#######GJR - garch in mean with t-student distribution

#s <- ugarchspec(mean.model=list(armaOrder=c(0,0),
#                archm=T,
#                archpow=2),
#                variance.model=list(model="gjrGARCH"),
#                distribution.model="sstd")
#m<- ugarchfit(data=trt,spec=s)
#m
#plot(m,which="all")

stargazer(, , )