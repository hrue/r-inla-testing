inla.setOption(num.threads = "1:1")
n = 1000
beta = 1.2
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

formula = Y ~ 1 + x
r=inla(formula,
       family ="weibullsurv",
       data=data,
       control.fixed = list(prec = 1), 
       control.family = list(variant = variant))
r <- inla.rerun(r)

sc <- 10
data$Y <- inla.surv(y/sc, event)
data$sc <- rep(sc, n)
formula <- update(formula, . ~ . + offset(log(sc)))
rr=inla(formula,
       family ="weibullsurv",
       data=data,
       control.fixed = list(prec = 1), 
       control.family = list(variant = variant))
rr <- inla.rerun(rr)

print(r$summary.fixed - rr$summary.fixed)
print(r$summary.hyperpar - rr$summary.hyperpar)
print(round(dig = 5, r$mlik - rr$mlik + n * log(sc)))
