n = 3000
n2 = n %/% 2L
strata = c(rep(1, n2), rep(2, n2))
p = c(0.1, 0.3)
size = 3
y = numeric(n)
x = runif(n)
x = x - mean(x)
intercept = 2

for(i in 1:n) {
    pp = p[strata[i]]
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
        family = "zeroinflatednbinomial1strata2",
        ##control.family = list(hyper = list(theta1 = list(param = c(1, 0.01)))), 
        strata = strata,
        verbose=TRUE)
