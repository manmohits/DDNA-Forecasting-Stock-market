## hngarchSim -
# Simulate a Heston Nandi Garch(1,1) Process:
# Symmetric Model - Parameters:
## model -
# Define the Model Parameters for a Heston-Nandi Option:
library(tseries)
library(xts)
library(rugarch)
library(fOptions)

data=as.xts(get.hist.quote(instrument = "aapl",start = "2017-12-01",end = "2018-12-01",quote="AdjClose"))
data<-data$Adjusted
#data
x=na.omit(diff(log(data)))

model = list(lambda = -0.5, omega = var(x), alpha = 0.1*var(x),
             beta = 0.1,gamma = 0, rf = 0)

ts = hngarchSim(model = model, n = 500, n.start = 0)
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
