S0=10; K=10;r=0.05; sigma=0.2 ; T=0.25;
St=S0*exp(norminv(u,r*T-sigma^2*T/2, sigma*sqrt(T)));
v=exp(-r*T)*max((ST-ex),0);
fn<-function(u,S0,strike,r,sigma,T){
  x<-S0*exp(qnorm(u,mean=r*T-sigma^2*T/2,sd=sigma*sqrt(T)))
  v<-exp(-r*T)*pmax((x-strike),0)
  v
}
u= rand(1,500000); mean(fn(u))

mean(fn(runif(500000), S0=10,strike=10,r=0.05,sigma=0.2,T=0.25))