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

model=list(lambda = 4, omega = model.coefficients[2], alpha = model.coefficients[3],
           beta = model.coefficients[4], gamma = 0, rf = 0)
mle = hngarchFit(model = model, x = y1, symmetric = TRUE)


## summary.hngarch -
# HN-GARCH Diagnostic Analysis:
par(mfrow = c(2, 1), cex = 0.75)

summary(mle)
## hngarchStats -
# HN-GARCH Moments:
hngarchStats(mle$model)
