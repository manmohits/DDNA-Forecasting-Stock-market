
### Example: Investigate the Smile Effect

# This program calculates for the estimated symmetric and asymmetric
# Garch(1,1) the implied volatility from the Black and Scholes Option
# Pricing formula assuming that the real market follows ideally the
# Heston Nandi Option Garch(1,1) model
### 

# Show the GBSVolatility function:
GBSVolatility
###

library(tseries)
library(xts)
library(rugarch)
library(fOptions)

data=as.xts(get.hist.quote(instrument = "aapl",start = "2017-12-01",end = "2018-12-01",quote="AdjClose"))
data<-data$Adjusted

y1=na.omit(diff(log(data)))

# Compute Smile - Parameters:
S  = 85:115; X = 100
Time.inDays = 126 
model = list(lambda=-0.5, omega=var(y1), alpha=0.1*var(y1), beta=0.1, gamma=0)
sigma = sqrt(252*(model$alpha+model$omega)/(1-model$beta))
sigma   
###

# Pricing the Put:
Put <- impVola <- NULL
for (i in 1:length(S)) {
  Price = HNGOption("p", model, S[i], X, Time.inDays, 0)$price
  Put = c(Put, Price) 
  Volatility = GBSVolatility(Price, "p", S[i], X, Time.inDays/252, 0, 0)
  impVola = c(impVola, Volatility)
  cat("\n\t", i, "\t", S[i], "\t", Price, "\t", Volatility) }
###

# Plot:
par(mfrow = c(2, 2), cex = 0.7)
plot(S/X, Put, 
     xlab = "S/X", ylab = "Price", main = "HN Put Price")
plot(S/X, impVola, 
     xlab = "S/X", ylab = "Volatility", main = "BS Implied Volatility")
### 
