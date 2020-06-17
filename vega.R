
estvega<-function(Z, S0, sigma, r, T, K){
  ST = S0*exp(r*T+sigma*sqrt(T)*Z-5*sigma^2*T)
  v=max(0, ST-K)
  v1=exp(-r*T)*(v*((Z^2-1)/sigma-sqrt(T)*Z))
  #vprime=matrix(1,nrow=1, ncol=length(Z))*(ST>K)
  
  vprime=matrix(1,nrow=1, ncol=length(Z))
  
  
  v2= exp(-r*T)*(vprime*ST*(sqrt(T)*Z-sigma*T))
  
 # vega<-rep(0, 2)
  
  vega=c(mean(v1), mean(v2))
  SE = c(sqrt(var(v1))/length(Z),  sqrt(var(v2))/length(Z))
  price=exp(-r*T)*mean(v)
  return(list(price, vega, SE))
}
#######################################################

#Z1=rnorm(n=10000)





estvega(rnorm(10000),10,0.2,0.05,0.25,9)

 
# ###########option price##########################################################################
# local({r <- getOption("repos")
# r["CRAN"] <- "http://cran.stat.sfu.ca/"
# options(repos=r)})
# 
# # Set a vector of strings: package names to use (and install, if necessary)
# pkg_list = c('zoo', 'tseries', 'MASS','stats','car','moments','fGarch', 'readxl','Ecdat',
#              'evir','AER','sandwich','fracdiff','longmemo','faraway','xts','forecast','nor1mix','bootstrap','rugarch',
#              'quantmod', 'rugarch', 'nnfor', 'forecast', 'fOptions', 'ggplot2') 
# 
# 
# # ensure existing required packages are up to date:
# update.packages(ask=FALSE, oldPkgs=pkg_list)
# 
# # Install packages if needed
# for (pkg in pkg_list)
# {
#   # Try loading the library.
#   if ( ! library(pkg, logical.return=TRUE, character.only=TRUE) )
#   {
#     # If the library cannot be loaded, install it; then load.
#     install.packages(pkg)
#     library(pkg, character.only=TRUE)
#   }
# }
# 
# library(fOptions)
# library(tseries)
# library(ggplot2)
# library(xts)
######################Monte Carlo Simulation####################################
# ######################Monte Carlo Simulation####################################
# callMonte.function<-function(r, S0, sigma, t, K){
#   N=252
#   j=N*t
#   s=sigma/sqrt(N)
#   mn=r/N-s^2/2
#   y<-rep(0, j+1)
#   y[1]=S0
#   y2=S0*exp(cumsum(rnorm(j, mean=mn, sd=s)))
#   y[1:j+1]<-y2
#   x=(0:j)/N
#   # plot(x, y, type="l",main="simulated return from call option", xlab="time in years", ylab="value of stock")
#   #abline(h=K, lty=2, lwd=1, col="red")
#   z=exp(-r*t)*max(y[j+1]-K,0)
#   return(z)
# }
# graphics.off()
# y1=as.xts(get.hist.quote(instrument = "aapl",start = "2014-12-09",end = "2018-11-28",quote="AdjClose"))
# y1<-y1$Adjusted
# ret=na.omit(diff(log(y1)))  ##compute returns
# ########### hngarch###############################################################
# ## hngarchFit -
# # HN-GARCH log likelihood Parameter Estimation:
# 
# model=list(lambda = -0.5, omega =var(ret) , alpha = 0.1*var(ret),
#            beta =0.1, gamma = 0, rf = 0)
# ret<-as.ts(ret)
# mle = hngarchFit(model = model, x = ret, symmetric = TRUE)
# mle
# sigma1<-mean(sqrt(mle$h)) # sigma is the average of the volatility
# volatility<-sqrt(252)*sigma1
# 
# 
# call1<-rep(0, 10000)
# for(i in 1:10000){
#   call1[i]<-callMonte.function(0.05, 255, volatility , 63/250, 260)
# }
# mean(call1)
# 
# call.monte.hn<-mean(call1)
