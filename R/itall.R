lag.plot2=function(data1,data2,max.lag=0){ 
   name1=paste(deparse(substitute(data1)),"(t-",sep="")
   name2=paste(deparse(substitute(data2)),"(t)",sep="")
   data1=as.ts(data1)
   data2=as.ts(data2)
   max.lag=as.integer(max.lag)
   prow=ceiling(sqrt(max.lag+1))
   pcol=ceiling((max.lag+1)/prow)
  par(mfrow=c(prow,pcol), mar=c(2.5, 4, 2.5, 1), cex.main=1.1, font.main=1)
  for(h in 0:max.lag){                         
  plot(lag(data1,-h), data2, xy.labels=F, main=paste(name1,h,")",sep=""), ylab=name2, xlab="")
  }
}

acf2=function(data,maxlag=NULL){
  num=length(data)
  if (is.null(maxlag)) maxlag=ceiling(10+sqrt(num))
  ACF=acf(data, maxlag, plot=F)$acf[-1]
  PACF=pacf(data, maxlag, plot=F)$acf
  LAG=1:maxlag/frequency(data)
  minA=min(ACF)
  minP=min(PACF)
  U=2/sqrt(num)
  L=-U
  minu=min(minA,minP,L)-.01
  par(mfrow=c(2,1), mar = c(3,3,2,0.8),
    oma = c(1,1.2,1,1), mgp = c(1.5,0.6,0))
  plot(LAG, ACF, type="h",ylim=c(minu,1), 
    main=paste("Series: ",deparse(substitute(data))))
    abline(h=c(0,L,U), lty=c(1,2,2), col=c(1,4,4))
  plot(LAG, PACF, type="h",ylim=c(minu,1))
    abline(h=c(0,L,U), lty=c(1,2,2), col=c(1,4,4))
  return(cbind(ACF, PACF)) 
}

sarima=function(data,p,d,q,P=0,D=0,Q=0,S=-1,tol=.001){ 
  n=length(data)
  constant=1:n   
  xmean=matrix(1,n,1) 
  if (d>0 & D>0) 
    fitit=arima(data, order=c(p,d,q), seasonal=list(order=c(P,D,Q), period=S), 
            optim.control=list(trace=1,REPORT=1,reltol=tol))
  if (d>0 & D==0)  
    fitit=arima(data, order=c(p,d,q), seasonal=list(order=c(P,D,Q), period=S),
            xreg=constant,include.mean=F, optim.control=list(trace=1,REPORT=1,reltol=tol))
  if (d==0 & D==0)
    fitit=arima(data, order=c(p,d,q), seasonal=list(order=c(P,D,Q), period=S),
            xreg=xmean,include.mean=F, optim.control=list(trace=1,REPORT=1,reltol=tol))
  if (d==0 & D>0)  
    fitit=arima(data, order=c(p,d,q), seasonal=list(order=c(P,D,Q), period=S),
            xreg=constant,include.mean=F, optim.control=list(trace=1,REPORT=1,reltol=tol))
  if (S < 0) goof=20 else goof=3*S
  tsdiag(fitit,gof.lag=goof)
  k=length(fitit$coef)
  BIC=log(fitit$sigma2)+(k*log(n)/n)
  AICc=log(fitit$sigma2)+((n+k)/(n-k-2))
  AIC=log(fitit$sigma2)+((n+2*k)/n)
  innov<<-fitit$resid
  list(fit=fitit, AIC=AIC, AICc=AICc, BIC=BIC)
}

sarima.for=function(xdata,nahead,p,d,q,P=0,D=0,Q=0,S=-1,tol=.001){ 
  data=as.ts(xdata) 
  n=length(data)
  constant=1:n
  xmean=matrix(1,n,1)
  if (d>0 & D>0) 
    fitit=arima(data, order=c(p,d,q), seasonal=list(order=c(P,D,Q), period=S))
  if (d>0 & D==0)  
    fitit=arima(data, order=c(p,d,q), seasonal=list(order=c(P,D,Q), period=S),
            xreg=constant,include.mean=F)
  if (d==0 & D==0)
    fitit=arima(data, order=c(p,d,q), seasonal=list(order=c(P,D,Q), period=S),
            xreg=xmean,include.mean=F)
  if (d==0 & D>0)  
    fitit=arima(data, order=c(p,d,q), seasonal=list(order=c(P,D,Q), period=S),
            xreg=constant,include.mean=F)
  if (d>0 & D>0)   nureg=NULL
  if (d>0 & D==0)  nureg=(n+1):(n+nahead)
  if (d==0 & D==0) nureg=matrix(1,nahead,1)
  if (d==0 & D>0)  nureg=(n+1):(n+nahead)
 fore=predict(fitit, n.ahead=nahead, newxreg=nureg)  
#-- graph:
  U = fore$pred + 2*fore$se
  L = fore$pred - 2*fore$se
   a=max(1,n-100)
  minx=min(data[a:n],L)
  maxx=max(data[a:n],U)
   t1=xy.coords(data)$x; 
   if(length(t1)<101) strt=t1[1] else strt=t1[length(t1)-100]
   t2=xy.coords(fore$pred)$x; 
   endd=t2[length(t2)]
   xllim=c(strt,endd)
  ts.plot(data,fore$pred,col=1:2, xlim=xllim, ylim=c(minx,maxx), ylab=deparse(substitute(xdata))) 
  lines(fore$pred, col="red", type="p")
  lines(U, col="blue", lty="dashed")
  lines(L, col="blue", lty="dashed")
#
  return(fore)
}

cat("  itall has been installed", "\n")