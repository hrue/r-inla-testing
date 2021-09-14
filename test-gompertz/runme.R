library(flexsurv)
library(INLA)

n <- 1000
alpha <- 1.0
intercept <- 1.1
beta <- 1.2
x <- rnorm(n, sd = 0.2)
event <- rep(1,n)
eta <- intercept + beta*x
mu <- exp(eta)
y <- rgompertz(n, rate = mu, shape = alpha)

r<-inla(y ~ 1 + x,
       family ="gompertz", data=data.frame(y, x))

r.surv<-inla(inla.surv(y, event) ~ 1 + x, 
             family ="gompertzsurv",
             data=data.frame(y, event, x))

