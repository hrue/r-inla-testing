n <- 10^5
x <- rnorm(n)
xx <- rnorm(n)
y <- rpois(n, exp(1+x+xx))

Rprof()
r <- inla(y ~ 1, data = data.frame(y, x, xx),
          control.compute = list(dic = TRUE, waic = TRUE, 
                                 return.marginals.predictor = TRUE),
          verbose = TRUE, keep = T)
Rprof(NULL)

