n <- 1000
y <- rnorm(n)
x <- rnorm(n)
xx <- rnorm(n)
idx <- 1:n
iidx <- n:1
formula <- y ~ -1 + f(x, model = "clinear", range = c(0, 1)) +
    f(idx) + f(iidx, copy = "idx")

r <- inla(formula,
          data = data.frame(y, x, xx, idx, iidx),
          control.family = list(hyper = list(prec = list(initial = 0, fixed = TRUE))),
          verbose = TRUE)
rr <- inla(formula,
          data = data.frame(y, x, xx, idx, iidx),
          control.family = list(hyper = list(prec = list(initial = 0, fixed = TRUE))),
          verbose = TRUE)

