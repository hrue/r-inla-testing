inla.setOption(inla.mode = "experimental" )

n = 100
x = rnorm(n,  sd = 0.2)
eta = 1 + x
E = runif(n, min = 0.1, max=10)

mu = E * exp(eta)
size = 3
prob = size/(size + mu)
y = rnbinom(n, size, mu=mu)

if (FALSE) {
    ## remove data to predict
    Y <- y
    Y[seq(1, n, by = 2)] <- NA
} else {
    Y <- y
}

r = inla(Y ~ 1 + x,
         data = data.frame(Y, x, E),
         family = "nbinomial",
         E=E,
         control.compute = list(config = TRUE, cpo = TRUE), 
         verbose = T)
summary(r)

## compute PIT values, the easy (approximative) way. we compuare with the cpo/pit values which
## are not the same (since data are remove), but its about the same with a lot of data (and a
## simple model like here).

xx <- inla.posterior.sample(1000, r)
fun <- function(E, Y) {
    ## theta[1] is 'size',  this must be verified from 'summary(r)'
    size <- theta[1]
    prob <- size / (size + E * exp(Predictor))
    return (pnbinom(Y, size = size, prob = prob))
}
## here we insert complete data
p <- rowMeans(inla.posterior.sample.eval(fun, xx, E = E, Y = y))

plot(p, r$cpo$pit, pch = 19)
abline(a = 0, b = 1, lwd = 3, col = "blue")

