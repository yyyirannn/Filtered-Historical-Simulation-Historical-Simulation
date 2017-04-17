rm(list=ls())

#step a
install.packages('quantmod')
library('quantmod')
getSymbols('BAC',from='2000-01-03', to='2017-03-24')
plot(BAC$BAC.Close)

#step b
ret1<-diff(log(BAC$BAC.Adjusted))
ret1<-ret1[-1,]
T<-length(ret1)
var1_hs<-numeric(T)
for (i in 501:T){
  var1_hs[i]<- -quantile(ret1[(i-500):(i-1)],probs=0.01)
}
fit1<- garchFit(formula= ~garch(1,1),data=ret1[(i-500):(i-1)],trace=FALSE)
plot(var1_hs, col='red',type='l',ylim=c(0,0.1))

#step c
ret1<-diff(log(BAC$BAC.Adjusted))
ret1<-ret1[-1,]
T<-length(ret1)
library('fGarch')
var1_hs<- numeric(T)
var1_fhs<-numeric(T)
for (i in 501:T){
  retwindow<- ret1[(i-500):(i-1)]
  var1_hs[i]<- -quantile(retwindow,probs=0.01)
  fit2<-garchFit( formula = ~garch(1, 1), data = retwindow, trace = FALSE)
  sigmapred<-predict(fit2,n.ahead=1)
  sigma<-sqrt(fit2@h.t)
  retstand <-retwindow/sigma
  var1_fhs[i]<- -sigmapred*quantile(retstand, probs=0.01)
}
plot(var1_fhs, col='red', type='l', ylim=c(0,0.1))
points(var1_hs, col='blue', type='l')

#step d
qqnorm(ret1)
qqline(ret1,col='black')

#step e
qqnorm(retstand)
qqline(retstand,col='red')

#step f
getSymbols('AAPL',from='2000-01-03', to='2017-03-24')
getSymbols('BP',from='2000-01-03', to='2017-03-24')
getSymbols('BAC',from='2000-01-03', to='2017-03-24')
ret2<-diff(log(AAPL$AAPL.Adjusted))
ret2<-ret2[-1,]
ret3<-diff(log(BP$BP.Adjusted))
ret3<-ret3[-1,]
ret4<-diff(log(BAC$BAC.Adjusted))
ret4<-ret4[-1,]
ret234<-ret2*1/3+ret3*1/3+ret4*1/3
T<-length(ret234)
var1_500<-numeric(T)
for (i in 501:T){
  var1_hs[i]<- -quantile(ret1[(i-500):(i-1)],probs=0.01)
}
plot(var1_hs,col='blue',type='l' ,ylim=c(0,0.1))

fit3<-garchFit( formula = ~garch(1, 1), data = retwindow, trace = FALSE)
sigma<-sqrt(fit3@h.t)
plot(var1_fhs, col='red', type='l', ylim=c(0,0.1))
points(var1_hs, col='blue', type='l')