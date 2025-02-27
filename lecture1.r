###generating plots in lecture note 1.
 
x=read.table("d-aapl0009.txt",header=T)  # Load Apple stock returns
dim(x)
x[1,]
y=ts(x[,2],frequency=252,start=c(2000,1)) # Create a time-series object in R.
plot(y,type='l',xlab='year',ylab='rtn')
title(main='Apple daily return: 2000 to 2009')

 par(mfcol=c(2,1))
 hist(y,main='Returns',nclass=50)
 d1=density(y)
 plot(d1$x,d1$y,xlab='returns',ylab='den',type='l')

 x=read.table("m-sp2609.txt",header=T)
 dim(x)
[1] 1008    3
 tdx=c(1:1008)/12+1926
 par(mfcol=c(2,1))
 plot(tdx,x[,3],xlab='year',ylab='index',type='l')
 plot(tdx,x[,2],xlab='year',ylab='return',type='l')

 x=read.table("m-tb3ms.txt",header=T)
 dim(x)
 y=read.table("m-tb6ms.txt",header=T)
 dim(y)
 dim(x)

 int=cbind(x[300:914,4],y[,4])
 tdx=(c(1:615)+10)/12+1958
 par(mfcol=c(1,1))
 max(int)
 plot(tdx,int[,1],xlab='year',ylab='rate',type='l',ylim=c(0,16.5))
 lines(tdx,int[,2],lty=2)
 
  par(mfcol=c(2,1))
 max(int)
 plot(tdx,int[,1],xlab='year',ylab='3m rate',type='l',ylim=c(0,16.5))
 plot(tdx,int[,2],xlab='year',ylab='6m rate',type='l',ylim=c(0,16.5))

 plot(tdx,int[,2]-int[,1],xlab='year',ylab='spread',type='l')
 abline(h=c(0))

 x=read.table("q-ko-earns8309.txt",header=T)
 dim(x)
 tdx=c(1:107)/4+1983
 plot(tdx,x[,3],xlab='year',ylab='earnings',type='l')
 title(main='EPS of Coca Cola: 1983-2009')
 points(tdx,x[,3])
 
 y=read.table("d-useu.txt",header=T)
 dim(y)
[1] 2807    4

 tdx=c(1:2807)/252+1999
 plot(tdx,y[,4],xlab='year',ylab='eu',type='l')
 title(main='Dollars per Euro')

 r=diff(log(y[,4]))
 plot(tdx[2:2807],r,xlab='year',ylab='rtn',type='l')
 title(main='ln-rtn: US-EU')

 hist(r,main='useu: ln-rtn',nclass=50)



### Galton height example

xx=matrix(scan(file="Galton_parent_child_height.txt"),ncol=2,byrow=T)
dim(xx)
 y=xx[,1] # child 
 x=xx[,2] # mid-parent 
 out=lm(y~x) 
  summary.lm(out)
  

  plot(x,y,ylab='child',xlab='mid-parent') 
  lines(x,x) 
  lines(x,out$fit) 
  title('Galton LS fit')
  plot(x,out$res, ylab='residual',xlab='mid-parent') 
  lines(x,x*0)
  title('Galton residual plot')
  
  ### CAPM example 
  
#par(mfrow=c(1,2))
 xx=read.table(file="SP500_and_3mTCM.txt",header=TRUE)
 yy=read.table(file="m_logret_10stocks.txt",header=TRUE)
 marketrtn=xx[,"sp500"]-0.01*xx[,"X3mTCM"]
 aaplrtn=yy[,"AAPL"]-0.01*xx[,"X3mTCM"]
 plot(marketrtn,aaplrtn,xlab='SP500',ylab='AAPL',ylim=c(-0.45,0.22))
 title('excess return: AAPL vs SP500') 
 out1=lm(aaplrtn~marketrtn)
 lines(marketrtn,out1$fit) 
 summary.lm(out1) 

#Example Car emissions
 
 plot(jitter(Carbon) ~ jitter(City), xlab = "City (mpg)", 
      ylab = "Carbon footprint (tons per year)", data = fuel)
 fit <- lm(Carbon ~ City, data = fuel)
 abline(fit)
 
 summary(fit)
 
 res <- residuals(fit)
 plot(jitter(res) ~ jitter(City), ylab = "Residuals", xlab = "City", data = fuel)
 abline(h = 0)
 
 fit <- lm(Carbon ~ City, data = fuel)
 fitted(fit)[1] # check fit$fitted[1]
 fcast <- forecast(fit, newdata=data.frame(City=30))
 plot(fcast, xlab="City (mpg)", ylab="Carbon footprint (tons per year)")
 points(jitter(Carbon) ~ jitter(City), data = fuel)
 
 summary(fit)
 
 confint(fit, level = 0.95)
 
 par(mfrow=c(1,2))
 fit2 <- lm(log(Carbon) ~ log(City), data=fuel)
 plot(jitter(Carbon) ~ jitter(City), xlab="City (mpg)",
      ylab="Carbon footprint (tonnes per year)", data=fuel)
 lines(1:50, exp(fit2$coef[1]+fit2$coef[2]*log(1:50)))
 plot(log(jitter(Carbon)) ~ log(jitter(City)), 
      xlab="log City mpg", ylab="log carbon footprint", data=fuel)
 abline(fit2)
 
 par(mfrow=c(1,1))
 res <- residuals(fit2)
 plot(jitter(res, amount = 0.005) ~ jitter(log(City)), 
      ylab = "Residuals", xlab = "log(City)", data = fuel)
 

######  multiple regression   #######

library(fpp)

pairs(credit[,-(4:5)])

## Transformation of the data.

creditlog <- data.frame(score=credit$score, 
                        log.savings=log(credit$savings+1), 
                        log.income=log(credit$income+1), 
                        log.address=log(credit$time.address+1),
                        log.employed=log(credit$time.employed+1), 
                        fte=credit$fte, single=credit$single)
pairs(creditlog[,1:5])

## Multiple regression of the transformed data.

fit <- step(lm(score ~ log.savings + log.income + log.address 
               + log.employed + single, data=creditlog))
summary(fit)

plot(fitted(fit), creditlog$score,
     ylab="Score", xlab="Predicted score")

## Multiple regression using tslm.

beer2 <- window(ausbeer,start=1992,end=2006-.1)
fit <- tslm(beer2 ~ trend + season)
summary(fit)

## multiple regression with dummy quaterly variables.

x1=1:56
x2=rep(c(0,1,0,0),14)
x3=rep(c(0,0,1,0),14)
x4=rep(c(0,0,0,1),14)
f1=lm(beer2~x1+x2+x3+x4)
summary(f1)

## residuals for the regression.
fit <- tslm(beer2 ~ trend + season)
res <- residuals(fit)
par(mfrow=c(1,2))
plot(res, ylab="Residuals",xlab="Year")
Acf(res, main="ACF of residuals")
Box.test(res)


### Fama & French example

ff1=read.table(file="F-F_Research_Data_Factors.txt",header=TRUE)
ff2=read.table(file="F-F_Momentum_Factor.txt",header=TRUE) 
pp=read.table(file="25_Portfolios_5x5_vw.txt",header=TRUE) 
tt=517:876 ## 1970.1 to 1999.12
y=pp[tt,8]-ff1[tt,5]	##excess return of portfolio 22
xx=as.matrix(cbind(ff1[tt,2:4],ff2[tt,2])) ## form X matrix
out1=lm(y~xx)
summary.lm(out1)

