GAMMA = function(a, b=0) {
    return (pgamma(b, a, lower = FALSE) * gamma(a))
}
F = function(x, lambda) {
    x[x == 0] = .Machine$double.eps
    return (GAMMA(x, lambda)/GAMMA(x))
}

df.discrete = function(x, lambda) {
    return (F(x+1, lambda) - F(x, lambda))
}

df.cont = function(x, lambda, h = 1e-6) {
    n = length(x)
    res = numeric(n)
    xx = x[x > h]
    if (length(xx) > 0) {
        res[x > h] = ((F(xx+h, lambda) - F(xx-h, lambda))/(2*h))
    }
    xx = x[x <= h]
    if (length(xx) > 0) {
        res[x <= h] = ((F(xx+h, lambda) - F(xx, lambda))/h)
    }
    return (res)
}

