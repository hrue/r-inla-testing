
ld = function(x, sd = 0.1, y=1) {
    eta = x
    xx = sd * seq(-5, 5, by = 0.01)
    ds = sum(dpois(y, lambda = exp(x + xx))*dnorm(xx, sd = sd)) * diff(xx)[1]
    return(log(ds))
}

