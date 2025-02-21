library(fBasics)

da=read.table("dgnp82.txt") 

x=da[,1]

par(mfcol=c(2,2)) 

plot(x,type="l") 
plot(x[1:175],x[2:176]) 
plot(x[1:174],x[3:176])
acf(x,lag=12)

par(mfcol=c(1,1)) 
pacf(x,lag.max=12)

Box.test(x,lag=10,type="Ljung")

Box-Ljung test

data:  x 
X-squared = 43.2345, df = 10, p-value =
  4.515e-06

m1=ar(x,method="mle")

m1

Call:
  ar(x = x, method = "mle")

Coefficients:
  1        2        3  
0.3480   0.1793  -0.1423  

Order selected 3  sigma^2 estimated as  9.427e-05 


names(m1)

plot(m1$resid,type="l")

Box.test(m1$resid,lag=10,type="Ljung")

Box-Ljung test

data:  m1$resid 
X-squared = 7.0808, df = 10, p-value =
  0.7178

m2=arima(x,order=c(3,0,0))

m2

Series: x 
ARIMA(3,0,0) with non-zero mean 

Coefficients:
  ar1     ar2      ar3  intercept
0.3480  0.1793  -0.1423     0.0077
s.e.  0.0745  0.0778   0.0745     0.0012

sigma^2 estimated as 9.427e-05:  log likelihood=565.84
AIC=-1121.68   AICc=-1121.33   BIC=-1105.83

names(m2)

Box.test(m2$residuals,lag=10,type="Ljung")

Box-Ljung test

data:  m2$residuals 
X-squared = 7.0169, df = 10, p-value = 0.7239


plot(m2$residuals,type="l")

tsdiag(m2)

p1=c(1,-m2$coef[1:3])

roots=polyroot(p1)

roots

Mod(roots)
[1] 1.913308 1.920152 1.913308

predict(m2,8)

$pred
Time Series:
  Start = 177 
End = 184 
Frequency = 1 
[1] 0.001236254 0.004555519 0.007454906 0.007958518
[5] 0.008181442 0.007936845 0.007820046 0.007703826

$se
Time Series:
  Start = 177 
End = 184 
Frequency = 1 
[1] 0.009709322 0.010280510 0.010686305 0.010688994
[5] 0.010689733 0.010694771 0.010695511 0.010696190


######

da=read.table("q-gnp4710.txt",header=T)
head(da)
G=da$VALUE
plot(G)
LG=log(G)
plot(LG)
gnp=diff(LG)
dim(da)
tdx=c(1:253)/4+1947 # create the time index
par(mfcol=c(2,1))
plot(tdx,G,xlab='year',ylab='GNP',type='l')
plot(tdx[2:253],gnp,type='l',xlab='year',ylab='growth') 
acf(gnp,lag=12)
pacf(gnp,lag=12) # compute PACF
m1=arima(gnp,order=c(3,0,0))
m1
tsdiag(m1,gof=12)  # model checking discussed later
p1=c(1,-m1$coef[1:3]) # set-up the polynomial
r1=polyroot(p1) # solve the polynomial equation
r1
Mod(r1)

mm1=ar(gnp,method='mle')
mm1$order # Find the identified order 
names(mm1)
print(mm1$aic,digits=3)
aic=mm1$aic  # For plotting below.
length(aic)
plot(c(0:12),aic,type='h',xlab='order',ylab='aic')
lines(0:12,aic,lty=2)

##### example on SPY returns.

library(quantmod)
getSymbols("SPY")

getSymbols("SPY",from='2010-10-01',src="yahoo")
dim(SPY)

head(SPY)
tail(SPY)

data=SPY[,6]    

logrtn=dailyReturn(data,type="log") 
simplertn=dailyReturn(data)

hist(simplertn)
hist(logrtn)
par(mfrow=c(2,1))
hist(simplertn, freq=F,xlim=c(-0.05,0.05))
hist(logrtn,freq=F,xlim=c(-0.05,0.05))

par(mfrow=c(1,1))

# compare daily and log return
plot(1:length(simplertn),simplertn,type="l")
lines(1:length(logrtn),logrtn,col="red")
lines(1:length(simplertn),simplertn,col="green")

## some stats can be accessed directly
mean(logrtn)
sd(logrtn)
skewness(logrtn)
kurtosis(logrtn)

t.test(logrtn)

qqnorm(logrtn)

acf(logrtn,lag=15) # Obtain the ACF plot

Box.test(logrtn,lag=10)

Box.test(logrtn,lag=10,type="Ljung")

model1=ar(as.vector(logrtn) ,method="mle") # Automatic AR fitting using AIC criterion.
model1    

names(model1)

plot(model1$resid,type='l')               

## checks residuals to see if they look like white noises now.
Box.test(model1$resid,lag=10,type='Ljung') 

model1$x.mean # Predicted overal mean value (daily)

## Another approach with order specified
# ARIMA has 3 parameters, they are the orders of ar, differencing and ma respectively. Since it's AR(2), the order is c(2,0,0). We will talk about differencing in a few lectures.
model2= arima(logrtn, order=c(4,0,0))
model2
model2$coef

## "tsdiag" gives a few diagnosis plots of the model. What do you find from these results?
tsdiag(model2)
Box.test(model2$residuals,lag=10,type='Ljung')
plot(model2$residuals,type='l')

## The following is the stationarity check.
poly1=c(1,-model2$coef[1:4])
roots=polyroot(poly1)
roots

Mod(roots)
## All roots lie outside the unit circle therefore stationary time series

## Prediction

predict(model2,10)
## predict 10 days ahead. Think how would you measure the error for prediction?

plot(predict(model2,20)$pred)
## what can you say about the prediction trend?

model2$coef
## check this number with the predicted values.

