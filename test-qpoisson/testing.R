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
    return (incG(x, lambda) /gamma(x))
}

fc = function(x,  lambda, h=1e-6) {
    return ((Fc(x+h, lambda)-Fc(x-h, lambda))/2/h)
}


find.lambda = function(quantile, alpha) {
    ## find lambda so that prob(X <= quantile) = alpha

    ans = optimize(
        function(log.lambda, quan, alpha) (Fc(quan,  exp(log.lambda)) - alpha)^2, 
        lower = log(0.001), upper = log(200), maximum = FALSE,
        quan = quantile,  alpha = alpha)
    return (exp(ans$minimum))
}
                    

x = seq(0,10, by=0.01)
f1 = splinefun(x, Fc(x, lambda=3))
f2 = splinefun(Fc(x, lambda=3), x)

