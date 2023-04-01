n = 100
alpha = 1.1
beta = 1.2
x = c(scale(runif(n)))
eta = 1+beta*x
lambda = exp(eta)

for(variant in 0:1) {
    y1 = rweibull(n,
                 shape= alpha,
                 scale= if (variant == 0)
                            lambda^(-1/alpha)
                        else
                            1/lambda)
    y2 = rweibull(n,
                 shape= alpha^2,
                 scale= if (variant == 0)
                            lambda^(-1/alpha)
                        else
                            1/lambda)

    na = rep(NA, n)
    event = rep(1,n)
    Y1 = inla.surv(c(y1, na), c(event, na))
    Y2 = inla.surv(c(na, y2), c(na, event))
    data = list(Y=list(Y1, Y2), x1=c(x, na), x2 = c(na, x),
                intercept = as.factor(rep(1:2, each=n)))

    formula = Y ~ -1 + intercept + x1 + x2
    r=inla(formula,
           family =c("weibull.surv", "weibull.surv"), 
           data=data,
           control.family = list(list(variant = variant), list(variant = variant)))
    print(summary(r))

}
