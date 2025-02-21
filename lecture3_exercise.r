
# 

AP <- AirPassengers
plot(AP)  # do a log transformation
LAP=log(AP)
LAP.hw <- HoltWinters(LAP)
plot(LAP.hw)
plot(LAP.hw$fitted)

LAP.hw$coefficients

#### manually compute the forecast for the next 3 periods

 coef=LAP.hw$coefficients
 coef[1]+coef[2]+coef[3]

6.108089 
 coef[1]+coef[2]*2+coef[4]

6.049299 
 coef[1]+coef[2]*3+coef[5]

6.162551 

library(forecast)
LAP.predict <- forecast(LAP.hw, h = 4*12)
plot(LAP.predict)


a=runif(1000)
b=exp(a)
c=exp(100*a)
cor(b,c)

cor(b,c, method='kendall')
cor(b,c, method='spearman')


### Simple simulated AR examples.

x=rep(0,100)
w=rnorm(100)
for (t in 2:100) x[t]<-0.6*x[t-1]+w[t]
layout(1:3)
plot(x,type="l")
acf(x)
pacf(x)

x.ar<-ar(x,method="mle")
x.ar$order
 # [1] 1
x.ar$ar

x.ar$ar+c(-2,2)*sqrt(x.ar$asy.var)



library(quantmod)
getSymbols("SPY")

getSymbols("SPY",from='2012-01-01',to='2017-02-01',src="yahoo")
dim(SPY)

head(SPY)
tail(SPY)

chartSeries(SPY)

chartSeries(SPY, subset='last 4 months')
chartSeries(SPY, subset='2013-01-01::2014-01-01')
chartSeries(SPY,theme="white")


getSymbols("UNRATE",src="FRED") ## download unemployment rates from FRED

head(UNRATE)

chartSeries(UNRATE,theme="white")

### now look at log returns ###

logrtn=diff(log(UNRATE$UNRATE))

chartSeries(logrtn, theme="white")

getSymbols("DEXUSEU",src="FRED") ## http://research.stlouisfed.org/fred2/categories/94?t=&at=exchange+rate&ob=pv&od=desc

head(DEXUSEU)

USEU.rtn=diff(log(DEXUSEU$DEXUSEU))

chartSeries(USEU.rtn, theme="white")

chartSeries(DEXUSEU, theme="white")

chartSeries(SPY,TA=NULL)   #no volume
chartSeries(SPY,TA=c(addVo(),addBBands()))  #add volume and Bollinger Bands from TTR

addMACD()   #  add MACD indicator to current chart


