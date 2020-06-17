###########option price##########################################################################
local({r <- getOption("repos")
r["CRAN"] <- "http://cran.stat.sfu.ca/"
options(repos=r)})

# Set a vector of strings: package names to use (and install, if necessary)
pkg_list = c('zoo', 'tseries', 'MASS','stats','car','moments','fGarch', 'readxl','Ecdat',
             'evir','AER','sandwich','fracdiff','longmemo','faraway','xts','forecast','nor1mix','bootstrap','rugarch',
             'quantmod', 'rugarch', 'nnfor', 'forecast', 'fOptions', 'ggplot2') 


# ensure existing required packages are up to date:
update.packages(ask=FALSE, oldPkgs=pkg_list)

# Install packages if needed
for (pkg in pkg_list)
{
  # Try loading the library.
  if ( ! library(pkg, logical.return=TRUE, character.only=TRUE) )
  {
    # If the library cannot be loaded, install it; then load.
    install.packages(pkg)
    library(pkg, character.only=TRUE)
  }
}

library(fOptions)
library(tseries)
library(ggplot2)
library(xts)
######################Monte Carlo Simulation####################################
callMonte.function<-function(r, S0, sigma, t, K){
  N=252
  j=N*t
  s=sigma/sqrt(N)
  mn=r/N-s^2/2
  y<-rep(0, j+1)
  y[1]=S0
  y2=S0*exp(cumsum(rnorm(j, mean=mn, sd=s)))
  y[1:j+1]<-y2
  x=(0:j)/N
  # plot(x, y, type="l",main="simulated return from call option", xlab="time in years", ylab="value of stock")
  #abline(h=K, lty=2, lwd=1, col="red")
  z=exp(-r*t)*max(K-y[j+1],0)
  return(z)
  }
graphics.off()
y1=as.xts(get.hist.quote(instrument = "aapl",start = "2014-12-09",end = "2018-11-28",quote="AdjClose"))
y1<-y1$Adjusted
ret=na.omit(diff(log(y1)))  ##compute returns
########### hngarch###############################################################
## hngarchFit -
# HN-GARCH log likelihood Parameter Estimation:

model=list(lambda = -0.5, omega =var(ret) , alpha = 0.1*var(ret),
           beta =0.1, gamma = 0, rf = 0)
ret<-as.ts(ret)
mle = hngarchFit(model = model, x = ret, symmetric = TRUE)
mle

######################################################################################Metrhod I
call.hn<-HNGOption(TypeFlag = "p", model= mle$model,S=166.47, X=180, Time.inDays=242, r.daily=0.05/252)
## Call Price using HNGOption function ###############################
# # Model Parameters:
    # lambda = -1/2
    # omega = model$omega
    # alpha = model$alpha
    # gamma = model$gamma + model$lambda + 1/2
    # beta = model$beta
    # sigma2 = (omega + alpha)/(1 - beta - alpha * gamma^2)
    # # Function to be integrated:
    # cphi0 = phi*complex(real = 0, imaginary = 1)
    # cphi = cphi0 + const
    # a = cphi * r.daily
    # b = lambda*cphi + cphi*cphi/2
    # for (i in 2:Time.inDays) {
        # a = a + cphi*r.daily + b*omega - log(1-2*alpha*b)/2
        # b = cphi*(lambda+gamma) - gamma^2/2 + beta*b +
            # 0.5*(cphi-gamma)^2/(1-2*alpha*b) }
    # fun = exp(-cphi0*log(X)+cphi*log(S)+a+b*sigma2)/cphi0/pi

#################################################################################Method II
sigma1<-mean(sqrt(mle$h)) # sigma is the average of the volatility
volatility<-sqrt(252)*sigma1


call1<-rep(0, 10000)
for(i in 1:10000){
  call1[i]<-callMonte.function(0.05, 166.47, volatility , 242/250, 180)
}
mean(call1)

call.monte.hn<-mean(call1)

###################################################################################Method III
sigma2<-sqrt(mle$sigma)
# Model Parameters:
    # lambda = -1/2
    # omega = model$omega
    # alpha = model$alpha
    # gamma = model$gamma + model$lambda + 1/2
    # beta = model$beta
    # sigma2 = (omega + alpha)/(1 - beta - alpha * gamma^2)
sigma2*sqrt(252)

call2<-rep(0, 10000)
for(i in 1:10000){
  call2[i]<-callMonte.function(0.05, 166.47,sigma2*sqrt(252) , 242/252, 180)
}
call.monte.hn2<-mean(call2)

#####################################################################################
############# nnetar ################################################################ Method IV
sd.ret<-sd(ret)
ret.abs<-abs(ret-mean(ret))
mean.abs<-mean(ret.abs)
ret.abs<-as.ts(ret.abs)
rho.hat<-mean.abs/sd.ret
nnetar.fit<-nnetar(ret.abs/rho.hat)
sigma.nnetar<-forecast(nnetar.fit, h=1)$mean

nnetar.fit
sigma.nnetar.fit<-na.omit(nnetar.fit$fitted)

mean.sigma.nnetar*sqrt(252)
####################################################################################################
mean.sigma.nnetar<-mean(sigma.nnetar.fit)
#################################################MC using the mean of nnet volatility ############
call3<-rep(0, 10000)
for(i in 1:10000){
  call3[i]<-callMonte.function(0.05, 166.47, mean.sigma.nnetar*sqrt(252) , 242/252, 180)
}
call.nnetar<-mean(call3)
call.monte.hn; call.monte.hn2; call.hn$price; call.nnetar

sigma2*sqrt(252); sigma2*sqrt(252); volatility
