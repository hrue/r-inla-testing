n = 3000
p = 1E-12 ## treated as zero
size = 10
y = numeric(n)
x = rnorm(n, sd = 0.3)
intercept = 0

for(i in 1:n) {
    m = exp(intercept + x[i])
    prob = size/(size + m)
    done <- FALSE
    while(!done) {
        y[i] = rnbinom(1, size=size, prob=prob)
        done <- (y[i] > 0)
    }
}

r = inla(y ~ 1 + x,
        data = data.frame(y, x),
        family = "zeroinflatednbinomial0",
        control.family = list(hyper = list(prob = list(initial = inla.link.logit(p),
                                                       fixed = TRUE))), 
        control.predictor=list(link=1),
        control.compute = list(config = TRUE), 
        verbose=!TRUE)
table(y)
summary(r)
