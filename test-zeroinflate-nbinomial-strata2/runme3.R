n = 3000
p = 0.999
size = 1
y = numeric(n)
x = runif(n)
x = x - mean(x)
intercept = 1

for(i in 1:n) {
    pp <- p
    if (runif(1) < pp) {
        y[i] = 0
    } else {
        m = exp(intercept + x[i])
        prob = size/(size + m)
        y[i] = rnbinom(1, size=size, prob=prob)
    }
}

r = inla(y ~ 1 + x,
        data = data.frame(y, x),
        family = "zeroinflatednbinomial0",
        keep = TRUE, 
        verbose=TRUE)
