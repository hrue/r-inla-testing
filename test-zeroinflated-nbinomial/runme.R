n = 3000
np <- 10
p = 0.1
size = 10
y = numeric(n)
x = rnorm(n)
intercept = 3

for(i in 1:n) {
    pp = p
    if (runif(1) < pp) {
        y[i] = 0
    } else {
        m = exp(intercept + x[i])
        prob = size/(size + m)
        y[i] = rnbinom(1, size=size, prob=prob)
    }
}

y[(n-np):n] <- NA

r = inla(y ~ 1 + x,
        data = data.frame(y, x),
        family = "zeroinflatednbinomial0",
        control.predictor=list(link=1),
        control.compute = list(config = TRUE), 
        verbose=!TRUE)

## there is a vignette discussing these functions
xx <- inla.posterior.sample(10^3, r)
fun <- function() {
    p0 <- theta[2]
    mu <- exp(Predictor)
    return (p0 + (1-p0) * mu)
}

fitted <- inla.posterior.sample.eval(fun, xx)
plot(rowMeans(fitted), y)
abline(a=0,b=1)
     
