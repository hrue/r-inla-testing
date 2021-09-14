invlink <- function(x, q)
    qgamma(1.0 - q, shape = exp(x)+1.0, rate=1)
