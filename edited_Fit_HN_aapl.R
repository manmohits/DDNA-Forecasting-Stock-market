library(tseries)
library(xts)
library(rugarch)
library(fOptions)

data=as.xts(get.hist.quote(instrument = "aapl",start = "2017-12-01",end = "2018-12-01",quote="AdjClose"))
data<-data$Adjusted

y1=na.omit(diff(log(data)))  ##compute returns

# 
# garch.norm=ugarchspec(mean.model = list(armaOrder=c(0,0)), 
#                       variance.model=list(garchOrder=c(1,1)), distribution="norm")
# 
# stock.garch.norm=ugarchfit(data=y1, spec=garch.norm)
# 
# show(stock.garch.norm)
# 
# model.coefficients<-as.numeric(coef(stock.garch.norm))
# model.coefficients[1]
# model.coefficients[2]
# model.coefficients[3]
# model.coefficients[4]

#fitting the model obtained from RuGARCH ******************************
# set.seed(4711)
# model = list(lambda = 0.7, omega = model.coefficients[2], alpha = model.coefficients[3],
#            beta = model.coefficients[4], gamma = 0, rf = 0.037/252)


   
    
model = list(lambda = -0.5, omega = var(y1), alpha = 0.1*var(y1),
                 beta =  0.1, gamma = 0, rf = 0)

ts = hngarchSim(model = model, n = 500, n.start = 100)
par(mfrow = c(2, 1), cex = 0.75)
ts.plot(ts, col = "steelblue", main = "HN Garch Symmetric Model")
grid()  
    
     ## hngarchFit -
    # HN-GARCH log likelihood Parameter Estimation:
    # To speed up, we start with the simulated model ...
mle = hngarchFit(model = model, x = ts, symmetric = TRUE)
#mle
## summary.hngarch -
# HN-GARCH Diagnostic Analysis:
par(mfrow = c(3, 1), cex = 0.75)
summary(mle)
## hngarchStats -
# HN-GARCH Moments:
hngarchStats(mle$model)





## model -
# Define the Model Parameters for a Heston-Nandi Option:
# model = list(lambda = -0.5, omega = 2.3e-6, alpha = 2.9e-6,
#              beta = 0.85, gamma = 184.25)
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
  
  
  HNG = c(HNG, HNGOption(TypeFlag, model = model, S = S, X = X,
                         Time.inDays = Time.inDays, r.daily = r.daily) $price)
  
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

