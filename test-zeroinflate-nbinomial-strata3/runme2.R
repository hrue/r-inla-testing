n = 30000
n3 = n %/% 3L
strata = c(rep(1, n3), rep(2, n3), rep(3, n3))
p = 0.2
sizes = c(3, 10, 20)
y = numeric(n)
x = runif(n)
x = x - mean(x)
intercept = 2

for(i in 1:n) {
    pp = p
    size = sizes[strata[i]]
    if (runif(1) < pp) {
        y[i] = 0
    } else {
        m = exp(intercept + x[i])
        prob = size/(size + m)
        y[i] = rnbinom(1, size=size, prob=prob)
    }
}

r = inla(y ~ 1 + x,
        data = data.frame(y, x, strata),
        family = "zeroinflatednbinomial1strata3",
        control.family = list(hyper = list(size3 = list(fixed=FALSE))), 
        strata = strata,
        verbose=TRUE)

