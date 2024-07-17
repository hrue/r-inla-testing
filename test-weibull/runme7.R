n = 10^5
beta = 0.2
alpha <- 1
x = c(scale(runif(n)))
eta = 1+beta*x
lambda = exp(eta)

variant <- 1
y = rweibull(n, shape= alpha, scale= 1/lambda)
event = rep(1,n)
data = list(Y=inla.surv(y, event),
            x=x,
            idx = 1:n, 
            intercept = rep(1, n))

formula = Y ~ -1 + intercept
r=inla(formula,
       family ="weibullsurv",
       data=data,
       ##control.inla = list(int.strategy = "eb"), 
       control.fixed = list(prec = 1), 
       control.family = list(variant = variant,
                             hyper =  list(alpha = list(param = 5))))
rr=inla(formula,
       family ="weibullsurv",
       data=data,
       ##control.inla = list(int.strategy = "eb"), 
       control.fixed = list(prec = 1), 
       control.family = list(variant = variant,
                             hyper =  list(alpha = list(param = 5))),
       inla.call = "inla.mkl.work")

r$mlik - rr$mlik
r$cpu.intern
rr$cpu.intern




























































































































































