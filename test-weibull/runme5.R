INLA:::inla.my.update()
inla.setOption(num.threads = "1:1",
               inla.call = "inla.mkl.work",
               inla.mode = "experimental")
n = 1000
beta = 1
alpha <- 1.2
x = c(scale(runif(n)))
eta = 1+beta*x
lambda = exp(eta)

variant <- 1
y = rweibull(n,
              shape= alpha,
              scale= if (variant == 0)
                         lambda^(-1/alpha)
                     else
                         1/lambda)
event = rep(1,n)
data = list(Y=inla.surv(y, event),
            x=x, 
            intercept = rep(1, n))

formula = Y ~ -1 + intercept + x
r=inla(formula,
       family ="weibullsurv",
       data=data,
       control.fixed = list(prec = 1), 
       control.family = list(variant = variant))

data = list(Y=inla.surv(y, event, cure = 1), 
            x=x, 
            intercept = rep(1, n))
formula = Y ~ -1 + intercept + x
rr=inla(formula,
       family ="weibullsurv",
       data=data,
       control.fixed = list(prec = 1), 
       control.family = list(variant = variant,
                             hyper = list(beta1 = list(
                                          initial = -12,
                                          fixed = TRUE))))

print(r$mlik -rr$mlik)
plot(r$internal.marginals.hyperpar[[1]], pch = 19)
lines(rr$internal.marginals.hyperpar[[1]], lwd = 2, col = "blue")

