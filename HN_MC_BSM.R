## model -
# Define the Model Parameters for a Heston-Nandi Option:
model = list(lambda = -0.5, omega = 2.3e-6, alpha = 2.9e-6,
             beta = 0.85, gamma = 184.25)
S = 165.93
X = 180
Time.inDays = 403
r.daily = 0.05/Time.inDays
sigma.daily = sqrt((model$omega + model$alpha) /
                     (1 - model$beta - model$alpha * model$gamma^2))
sigma.daily
rf=0.05

TTM=403/252


data.frame(S, X, r.daily, sigma.daily)
## HNGOption -
# Compute HNG Call-Put and compare with GBS Call-Put:
HNG = GBS = Diff = NULL
for (TypeFlag in c("c", "p")) {
  HNG = c(HNG, HNGOption(TypeFlag, model = model, S = S, X = X,
                         Time.inDays = Time.inDays, r.daily = r.daily)$price )
  GBS = c(GBS, GBSOption(TypeFlag, S = S, X = X, Time = Time.inDays,
                         r = r.daily, b = r.daily, sigma = sigma.daily)@price) }

### Simulate Lognormal RV ###
R<-(rf-0.5*sigma^2)*TTM
SD<-sigma*sqrt(TTM)
TTM.price<-stock*exp(R+SD*rnorm(num.sim,0,1))

### Calculate Call Option Price ###
TTM.call<-pmax(0,TTM.price-strike)
PV.call<-TTM.call*(exp(-rf*TTM))
#Monte Carlo Call is
mean(PV.call)

### Calculate Put Option Price ###
TTM.put<-pmax(0,strike-TTM.price)
PV.put<-TTM.put*(exp(-rf*TTM))
#Monte Carlo Put is
#mean(PV.put)

Options = cbind(HNG, GBS, Diff = round(100*(HNG-GBS)/GBS, digits=2))
row.names(Options) <- c("Call", "Put")
data.frame(Options)



