
library(tseries)
library(xts)
library(rugarch)
library(fOptions)

data=as.xts(get.hist.quote(instrument = "aapl",start = "2017-12-01",end = "2018-12-01",quote="AdjClose"))
data<-data$Adjusted
g=length(data)

y1=na.omit(diff(log(data)))


## model -
# Define the Model Parameters for a Heston-Nandi Option:


model = list(lambda = -0.5, omega = var(y1), alpha = 0.1*var(y1),
             beta = 0.1, gamma = 0, rf =0.05/129)


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




sigma.daily = sqrt((model$omega + model$alpha) /
                     (1 - model$beta - model$alpha * model$gamma^2))

sigma.daily
