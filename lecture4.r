
library(fBasics)

da=read.table("dgnp82.txt") 

x=da[,1]

par(mfcol=c(1,1)) 
pacf(x,lag.max=12)



m1=ar(x,method="mle")

names(m1)

plot(m1$resid,type="l")

Box.test(m1$resid,lag=10,type="Ljung")

m2=arima(x,order=c(3,0,0))

names(m2)

Box.test(m2$residuals,lag=10,type="Ljung")

plot(m2$residuals,type="l")

tsdiag(m2)


############### Compare AR(1) and AR(3)

m3=arima(x,order=c(1,0,0))
source("fore.R")
source("foreplot.R")
source("backtest.R")
length(x)
p1=fore(m2,x,170,6)	  ## AR(3) model, which is the preferred model. Use the first 170 points to estimate the model. Then predict 6 steps ahead.

	Time Series:
Start = 171 
End = 176 
Frequency = 1 
[1] 0.005894313 0.007204859 0.007903095 0.008129649 0.008137828 0.008078615
Time Series:
Start = 171 
End = 176 
Frequency = 1 
[1] 0.009802442 0.010350634 0.010733141 0.010733787 0.010733916 0.010740454

#forecast plot starting at 170. The forecasting origin is 170.
foreplot(p1,x,170,170)

p2=fore(m3,x,170,6)	 ##AR(1) model. 

	Time Series:
Start = 171 
End = 176 
Frequency = 1 
[1] 0.006622872 0.007494466 0.007814617 0.007932214 0.007975409 0.007991275
Time Series:
Start = 171 
End = 176 
Frequency = 1 
[1] 0.009994515 0.010647425 0.010732476 0.010743900 0.010745440 0.010745648

 #forecast plot
foreplot(p2,x,170,170)

#Backtest using AR(3)
 backtest(m2,x,150,1)
[1] "RMSE of out-of-sample forecasts"
[1] 0.005280406
[1] "Mean absolute error of out-of-sample forecasts"
[1] 0.00392484

#Backtest using AR(1)
 backtest(m3,x,150,1)
[1] "RMSE of out-of-sample forecasts"
[1] 0.005394823
[1] "Mean absolute error of out-of-sample forecasts"
[1] 0.003973319


###################################################################
An MA example
###################################################################

da=read.table("d-ibmvwew6202.txt",header=T)
dim(da)
[1] 10194 4
vw=log(1+da[,3])*100 # Compute percentage log returns of the vw index.  da[,3] contains the simple return.
acf(vw,lag.max=10)
m1=arima(vw,order=c(0,0,1)) # fits an MA(1) model
 m1	 #MA(1)
Call:
arima(x = vw, order = c(0, 0, 1))
Coefficients:
ma1 intercept
0.1465 0.0396 % The model is vw(t) = 0.0396+a(t)+0.1465a(t-1).
s.e. 0.0099 0.0100
sigma^2 estimated as 0.7785: log likelihood = -13188.48, aic = 26382.96

m2=arima(vw,order=c(2,0,0))

m3=arima(vw,order=c(5,0,0), fixed=c(NA,NA,0,0,0,NA))
> m2	#AR(2)
Call:
arima(x = vw, order = c(2, 0, 0))
Coefficients:
         ar1      ar2  intercept
      0.1447  -0.0323     0.0396
s.e.  0.0099   0.0099     0.0098

sigma^2 estimated as 0.7784:  log likelihood = -13187.83, aic = 26383.66


 tsdiag(m1)
 tsdiag(m2)
 predict(m1,5)

 $pred
Time Series:
Start = 10195
End = 10199
Frequency = 1
[1] 0.05036298 0.03960887 0.03960887 0.03960887 0.03960887
$se
Time Series:
Start = 10195
End = 10199
Frequency = 1
[1] 0.8823290 0.8917523 0.8917523 0.8917523 0.8917523


backtest(m1,vw,10000,1)
[1] "RMSE of out-of-sample forecasts"
[1] 1.689353
[1] "Mean absolute error of out-of-sample forecasts"
[1] 1.324739

backtest(m2,vw,10000,1)
[1] "RMSE of out-of-sample forecasts"
[1] 1.689947
[1] "Mean absolute error of out-of-sample forecasts"
[1] 1.324433

##### backtesting ######

library(quantmod)
getSymbols("SPY")

getSymbols("SPY",from='2010-10-01',src="yahoo")
dim(SPY)

head(SPY)
tail(SPY)

data=SPY[,6]    

logrtn=dailyReturn(data,type="log") 
simplertn=dailyReturn(data)

n=length(logrtn)-140
data1=logrtn[1:n]
auto.arima(data1)
m1=arima(data1,order=c(1,0,1))
m2=arima(data1,order=c(0,0,5))
m3=arima(data1,order=c(5,0,0))

count=0

for (i in 1:140)
{
  
  data=logrtn[i:n+i-1,]
  model=arima(data,order=c(5,0,0))
  pre=predict(model,1)[1]
  
  if(as.numeric(pre)*(as.numeric(logrtn[n+i,1]))>0)
    count=count+1
  
}
