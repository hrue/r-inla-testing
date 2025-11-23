n <- 100
x <- rnorm(n)
xx <- rnorm(n)
xx <- rnorm(n)
xxx <- rnorm(n)
y <- rnorm(n)

r <- inla(y ~ 1 + x + xx + xxx,
          data = data.frame(y, x, xx, xxx),
          family = "stdnormal",
          control.compute = list(config = TRUE))

do.sample <- function(n, r, nm) {
    s <- inla.posterior.sample(n, r)
    ss <- inla.posterior.sample.eval(nm, s)
    rownames(ss) <- nm
    return (ss)
}

do.sample(5, r, "x")
do.sample(5, r, c("x", "xx"))
do.sample(5, r, c("x", "xx", "xxx"))

