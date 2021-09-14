n = 100
alpha = 1.1
beta = 1.2
x = c(scale(runif(n)))
eta = 1+beta*x
lambda = exp(eta)

for(variant in 1) {
    y = rweibull(n,
                 shape= alpha,
                 scale= if (variant == 0)
                            lambda^(-1/alpha)
                        else
                            1/lambda)

    print(paste("VARIANT=", variant))
    event = rep(1,n)
    data = list(y=y, event=event, x=x)

    formula=inla.surv(y,event)~ x
    r.surv=inla(formula,
           family ="weibullsurv",
           data=data,
           control.compute = list(dic=TRUE), 
           control.family = list(list(variant = variant)))
    print("SURV")
    print(summary(r.surv))

    formula= y ~ x
    r.reg=inla(formula,
           family ="weibull",
           data=data, 
           control.compute = list(dic=TRUE), 
           control.family = list(list(variant = variant)))
    print("REGRESSION")
    print(summary(r.reg))
}
