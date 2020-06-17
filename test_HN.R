library(tseries)
library(xts)
library(rugarch)
library(fOptions)

data=as.xts(get.hist.quote(instrument = "aapl",start = "2017-12-01",end = "2018-12-01",quote="AdjClose"))
data1<-data$Adjusted
data1

y1=na.omit(diff(log(data)))  ##compute returns
# y1=diff(log(data))  ##compute returns

# y1
#y1=data2lnorm(data1, plot=TRUE, forceNA = TRUE, ...)
##can this log normal data be negative??


#garch.norm=ugarchspec(mean.model = list(armaOrder=c(0,0)), 
 #                     variance.model=list(garchOrder=c(1,1)), distribution="norm")

#stock.garch.norm=ugarchfit(data=data1, spec=garch.norm)
#stock.garch.norm=ugarchfit(data=data1, spec=ugarchspec())

#show(stock.garch.norm)
#mean looks good
#LogLikelihood is way to wrong -772.0494


#model.coefficients<-as.numeric(coef(stock.garch.norm))


mle = hngarchFit(y1, model = list(lambda= -0.5, omega = var(y1), alpha = 0.1* var(y1), beta =0.1, gamma= 0, rf = 0), symmetric = TRUE, trace = FALSE, title = NULL, description = NULL)


#mle = hngarchFit(model = model, x = y1, symmetric = TRUE)
show(mle)


par(mfrow = c(2, 1), cex = 0.75)

summary(mle)

omega
model$alpha
model$beta
model$gamma


S1 = 165.93
X1 = 180

Time.inDays = 120
r.daily = 0.05/Time.inDays
sigma.daily = sqrt((model$omega + model$alpha) / (1 - model$beta - model$alpha * model$gamma^2))

sigma.daily= 0.20

data.frame(S, X, r.daily, sigma.daily)
## HNGOption -
# Compute HNG Call-Put and compare with GBS Call-Put:
HNG = GBS = Diff = NULL
for (TypeFlag in c("c", "p")) {-
  HNG = c(HNG, HNGOption(TypeFlag, model = model, S = S, X = X,
                         Time.inDays = Time.inDays, r.daily = r.daily)$price )
  GBS = c(GBS, GBSOption(TypeFlag, S = S1, X = X1, Time = Time.inDays,
                         r = r.daily, b = r.daily, sigma = sigma.daily)@price) }

Options = cbind(HNG, GBS, Diff = round(100*(HNG-GBS)/GBS, digits=2))
row.names(Options) <- c("Call", "Put")
data.frame(Options)
