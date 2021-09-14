incG = function(x, lambda=1) {
    ret = numeric(length(x))
    idx.0 = which(x <= 0)
    idx.n0 = which(x > 0)
    ret[idx.0] = if (lambda < 0) 1 else 0
    xx = x[idx.n0]
    if (length(xx) > 0)
        ret[idx.n0] = (1-pgamma(lambda, shape=xx, scale=1))*gamma(xx)
    return(ret)
}

Fc = function(x, lambda) {
    ##return (incG(x, lambda) /incG(x, lambda=-1))
    return (incG(x, lambda) /gamma(x))
}
fc = function(x,  lambda, h=1e-5) {
    return ((Fc(x+h, lambda)-Fc(x-h, lambda))/(2*h))
}


find.lambda = function(quantile, alpha) {
    ## find lambda so that prob(X <= quantile) = alpha
    ans = optimize(
        function(log.lambda, quan, alpha) (Fc(quan,  exp(log.lambda)) - alpha)^2, 
        lower = log(0.001), upper = log(200), maximum = FALSE,
        quan = quantile,  alpha = alpha)
    return (exp(ans$minimum))
}
                    

lambda=3.123
x = seq(0.001, 10, by=0.01)
f1 = splinefun(x, Fc(x, lambda))
f2 = splinefun(Fc(x, lambda), x)


plot(x, fc(x, lambda),  type="l")
points(0:10 + 0.5,  dpois(0:10, lambda))

n=10^6
y = f2(runif(n))
r = inla(y ~ 1,  data = data.frame(y), family = "contpoisson")

