## model -
# Define the Model Parameters for a Heston-Nandi Option:
library(tseries)
library(xts)
library(rugarch)
library(fOptions)

data=as.xts(get.hist.quote(instrument = "aapl",start = "2017-12-01",end = "2018-12-01",quote="AdjClose"))
data<-data$Adjusted
#data
xx=na.omit(diff(log(data))) 

model = list(lambda = -0.5, omega = var(xx), alpha = 0.1*var(xx),
             beta = 0.1,gamma = 0, rf = 0)


S = X = 100
Time.inDays = 252
r.daily = 0.05/Time.inDays
sigma.daily = sqrt((model$omega + model$alpha) /
                     (1 - model$beta - model$alpha * model$gamma^2))


data.frame(S, X, r.daily, sigma.daily)
## HNGOption -
# Compute HNG Call-Put and compare with GBS Call-Put:
HNG = GBS = Diff = NULL
for (TypeFlag in c("c", "p")) {
  #options need another loop for multiple values of option as 
  HNG = c(HNG, HNGOption(TypeFlag, model = model, S = S, X = X,
                         Time.inDays = Time.inDays, r.daily = r.daily)$price )
  GBS = c(GBS, GBSOption(TypeFlag, S = S, X = X, Time = Time.inDays,
                         r = r.daily, b = r.daily, sigma = sigma.daily)@price) }
Options = cbind(HNG, GBS, Diff = round(100*(HNG-GBS)/GBS, digits=2))
row.names(Options) <- c("Call", "Put")
data.frame(Options)
## HNGGreeks -
# Compute HNG Greeks and compare with GBS Greeks:
Selection = c("Delta", "Gamma")
HNG = GBS = NULL
for (i in 1:2){
  HNG = c(HNG, HNGGreeks(Selection[i], TypeFlag = "c", model = model,
                         S = 100, X = 100, Time = Time.inDays, r = r.daily) )
  GBS = c(GBS, GBSGreeks(Selection[i], TypeFlag = "c", S = 100, X = 100,
                         Time = Time.inDays, r = r.daily, b = r.daily, sigma = sigma.daily) ) }
Greeks = cbind(HNG, GBS, Diff = round(100*(HNG-GBS)/GBS, digits = 2))
row.names(Greeks) <- Selection
data.frame(Greeks)