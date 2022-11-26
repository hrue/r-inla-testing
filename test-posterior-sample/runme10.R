n <- 100
x <- rnorm(n)
xx <- rnorm(n)
y <- rnorm(n)

r <- inla(y ~ 1 + x*xx,
          data = data.frame(y, x, xx),
          control.compute = list(config = TRUE))

fun <- function() {
    return (c(x, xx, get("x:xx")))
}

xx <- inla.posterior.sample(1000, r)
coeffs <- inla.posterior.sample.eval(fun, xx)
