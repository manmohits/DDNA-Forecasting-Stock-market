library(tseries)
library(xts)
library(rugarch)
library(fOptions)

data=as.xts(get.hist.quote(instrument = "msft",start = "2018-11-01",end = "2018-12-01",quote="AdjClose"))
#^GSPC
#msft
#ibm
#aapl
data<-data$Adjusted
g=length(data)

data
y1=na.omit(diff(log(data)))  ##compute returns

# 
# 
# 
# rf = 0.04/252  
 set.seed(100)
 # model = list(lambda = 1.7686, omega = , alpha = 4.3859e-06,
 #               beta = 0.8733, gamma = 140.5724, rf = 0.035/252)
# 
# model = list(lambda = 1.7686, omega = 0, alpha = 0.1*var(y1),
#              beta =  4.3859e-06, gamma = 140.5724, rf = rf)
# 
# 
# ts = hngarchSim(model = model, n = 7570, n.start = 0)
# 

#spot rate = yield to maturity
 
S = 165.93 
X= 170
Time.inDays = 123
r.daily = 0.0/Time.inDays

model = list(lambda = -0.5, omega = var(y1), alpha = 0.1*var(y1),
           beta = 0.1, gamma = 0, rf =r.daily)


# hngarchSim -
# Simulate a Heston Nandi Garch(1,1) Process:
# Symmetric Model - Parameters:
# model = list(lambda = 4, omega = 8e-5, alpha = 6e-5,
#              beta = 0.7, gamma = 0, rf = 0)

ts = hngarchSim(model = model, n = g, n.start = 0)
par(mfrow = c(2, 1), cex = 0.75)
ts.plot(ts, col = "steelblue", main = "HN Garch Symmetric Model")
grid()

## hngarchFit -
# HN-GARCH log likelihood Parameter Estimation:
# To speed up, we start with the simulated model ...

mle = hngarchFit(model = model, x = ts, symmetric = TRUE)
mle
## summary.hngarch -
# HN-GARCH Diagnostic Analysis:
par(mfrow = c(3, 1), cex = 0.75)
summary(mle)
## hngarchStats -
# HN-GARCH Moments:
hngarchStats(mle$model)


#2

###

# List the R Function:
HNGOption
###

# Compute the Option Price:
# Call:
HNGOption(TypeFlag = "c", model, S, X, Time.inDays, r.daily)
# Put:
HNGOption(TypeFlag = "p", model, S, X, Time.inDays, r.daily)
###