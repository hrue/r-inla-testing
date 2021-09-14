giveme.lambda = function(q, quantile, variant, alpha)
{
    if (variant == 0) {
        return (-1/q^alpha * log(1-quantile))
    } else if (variant == 1) {
        return (1/q * (-log(1-quantile))^(1/alpha))
    } else {
        stop("ERROR")
    }
}

n = 10
alpha = 1.1
beta = 1.2
intercept = 1
x = c(scale(runif(n)))
eta = 1+beta*x
quantile = 0.99

for(variant in 0:1) {
    lambda = giveme.lambda(q = exp(eta),
                           quantile = quantile,
                           variant = variant,
                           alpha = alpha)
    y = rweibull(n,
                 shape= alpha,
                 scale= if (variant == 0)
                            lambda^(-1/alpha)
                        else
                            1/lambda)

    print(paste("VARIANT=", variant))
    event = rep(1,n)
    data = list(y=y, event=event, x=x, off = eta)

    formula=inla.surv(y,event)~ -1 + offset(off)
    r=inla(formula,
           family ="weibullsurv",
           data=data,
           control.family = list(list(variant = variant,
                                      control.link = list(
                                          quantile = quantile,
                                          model = "quantile"),
                                      hyper = list(alpha = list(initial = log(alpha),  fixed=TRUE)))), 
           control.inla = list(int.strategy = "eb"), 
           control.predictor = list(compute=TRUE, link=1))
           
    print("SURV")
    print(summary(r))

    formula= y ~ -1 + offset(off)
    rr=inla(formula,
           family ="weibull",
           data=data, 
           control.family = list(list(variant = variant,
                                      control.link = list(
                                          quantile = quantile,
                                          model = "quantile"),
                                      hyper = list(alpha = list(initial = log(alpha),  fixed=TRUE)))), 
           control.inla = list(int.strategy = "eb"), 
           control.predictor = list(compute=TRUE, link=1))
    print("REGRESSION")
    print(summary(rr))
}
