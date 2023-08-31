n <- 20
x <- rnorm(n)
xx <- rnorm(n)
eta <-  1 + x + xx

y <- rpois(n, lambda = exp(eta))
 
lc1 = inla.make.lincomb("(Intercept)" = 1, x = 2, xx = 3)
names(lc1) <- "lc1"
lc2 = inla.make.lincomb("(Intercept)" = -1, x = -2, xx = -3)
names(lc2) <- "lc2"
r = inla(y ~ 1 + x + xx, data = data.frame(y, x, xx),
         family = "poisson",
         safe = FALSE, verbose = T, 
         lincomb = c(lc1, lc2))
r$summary.lincomb.derived
