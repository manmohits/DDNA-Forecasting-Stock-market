gradientR<-function(y, X, epsilon,eta, iters){
  epsilon = 0.0001
  X = as.matrix(data.frame(rep(1,length(y)),X))
  N= dim(X)[1]
  print("Initialize parameters...")
  theta.init = as.matrix(rnorm(n=dim(X)[2], mean=0,sd = 1)) # Initialize theta
  theta.init = t(theta.init)
  e = t(y) - theta.init%*%t(X)
  grad.init = -(2/N)%*%(e)%*%X
  theta = theta.init - eta*(1/N)*grad.init
  l2loss = c()
  for(i in 1:iters){
    l2loss = c(l2loss,sqrt(sum((t(y) - theta%*%t(X))^2)))
    e = t(y) - theta%*%t(X)
    grad = -(2/N)%*%e%*%X
    theta = theta - eta*(2/N)*grad
    if(sqrt(sum(grad^2)) <= epsilon){
      break
    }
  }
  print("Algorithm converged")
  print(paste("Final gradient norm is",sqrt(sum(grad^2))))
  values<-list("coef" = t(theta), "l2loss" = l2loss)
  return(values)
}

normalest <- function(y, X){
  X = data.frame(rep(1,length(y)),X)
  X = as.matrix(X)
  theta = solve(t(X)%*%X)%*%t(X)%*%y
  return(theta)
}

y = rnorm(n = 10000, mean = 0, sd = 1)
x1 = rnorm(n = 10000, mean = 0, sd = 1)
x2 = rnorm(n = 10000, mean = 0, sd = 1)
x3 = rnorm(n = 10000, mean = 0, sd = 1)
x4 = rnorm(n = 10000, mean = 0, sd = 1)
x5 = rnorm(n = 10000, mean = 0, sd = 1)

ptm <- proc.time()
gdec.eta1 = gradientR(y = y, X = data.frame(x1,x2,x3, x4,x5), eta = 100, iters = 1000)

proc.time() - ptm

normalest(y=y, X = data.frame(x1,x2,x3,x4,x5))

gdec.eta1$coef

plot(1:length(gdec.eta1$l2loss),gdec.eta1$l2loss,xlab = "Epoch", ylab = "L2-loss")
lines(1:length(gdec.eta1$l2loss),gdec.eta1$l2loss)

plot(1:length(gdec.eta1$l2loss),gdec.eta1$l2loss,xlab = "Epoch", ylab = "L2-loss")
lines(1:length(gdec.eta1$l2loss),gdec.eta1$l2loss)