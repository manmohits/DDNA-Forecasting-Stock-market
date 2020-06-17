library(tseries)
library(xts)
library(rugarch)
library(fOptions)

data=as.xts(get.hist.quote(instrument = "aapl",start = "2017-12-01",end = "2018-12-01",quote="AdjClose"))
data<-data$Adjusted
data
y1=na.omit(diff(log(data)))  ##compute returns


garch.norm=ugarchspec(mean.model = list(armaOrder=c(0,0)), 
                      variance.model=list(garchOrder=c(1,1)), distribution="norm")

stock.garch.norm=ugarchfit(data=y1, spec=garch.norm)

show(stock.garch.norm)

model.coefficients<-as.numeric(coef(stock.garch.norm))
model.coefficients[1]
model.coefficients[2]
model.coefficients[3]
model.coefficients[4]

#fitting the model obtained from RuGARCH ******************************
set.seed(4711)
model = list(lambda = 0.7, omega = model.coefficients[2], alpha = model.coefficients[3],
           beta = model.coefficients[4], gamma = 0, rf = 0.037/252)

# 
# model = list(lambda = 0.7, omega = 1.6e-6, alpha = 1e-6, 
#              beta = 0.92, gamma = 0, rf = 0.037/252)
ts.sym = hngarchSim(model, n = 755, n.start = 100)
par(mfcol = c(3, 2), cex = 0.5)
ts.plot(ts.sym, main = "Symmetric Data")
###
#*********************************************************************

# Fit an Asymmetric HN-GARCH(1,1) Process:
set.seed(4711)
model = list(lambda = 0.2, omega = 5.0e-6, alpha = 1e-6, 
             beta = 0.59, gamma = 421, rf = 0.037/252)
ts.asym = hngarchSim(model, n = 755, n.start = 100)
ts.plot(ts.asym, main = "Asymmetric Data")
# Plot Both:
ts.plot(ts.asym, main = "Both Data Sets")
lines(ts.sym, col = "red")
###

# ACF Plots:
result = acf(abs(ts.sym), main = "ACF: Symmetric Data")
result = acf(abs(ts.asym), main = "ACF: Asymmetric Data")
###

