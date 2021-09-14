n <- 100
X <- matrix(rnorm(n^2), n, 2)
x <- X[, 1]
xx <- X[, 2]
xxx <- x*xx

y <- 1 + 2*x + 3*xx + 4*xxx + rnorm(n, sd = 0.01)

r <- inla(y ~ X[, 1]*X[, 2],
          data = list(y = y, X = X),
          control.compute = list(config = TRUE))
print(round(dig = 4, r$summary.fixed[,"mean"]))

sam <- inla.posterior.sample(100, r)
sam.extract <- inla.posterior.sample.eval(
    (function(...) {
        beta.1 <- get("X[, 1]")
        beta.2 <- get("X[, 2]")
        beta.12 <- get("X[, 1]:X[, 2]")
        return(c(Intercept, beta.1, beta.2, beta.12))
    }), sam)
print(round(dig = 4, rowMeans(sam.extract)))

r <- inla(y ~ x + xx + xxx,
          data = list(y = y, x = x, xx = xx, xxx = xxx), 
          control.compute = list(config = TRUE))

sam <- inla.posterior.sample(100, r)
sam.extract <- inla.posterior.sample.eval(
    (function(...) {
        return(c(Intercept, x, xx, xxx))
    }), sam)
print(round(dig = 4, rowMeans(sam.extract)))

r <- inla(y ~ x*xx,
          data = list(y = y, x = x, xx = xx), 
          control.compute = list(config = TRUE))

sam <- inla.posterior.sample(100, r)
sam.extract <- inla.posterior.sample.eval(
    (function(...) {
        return(c(Intercept, x, xx, get("x:xx")))
    }), sam)
print(round(dig = 4, rowMeans(sam.extract)))
