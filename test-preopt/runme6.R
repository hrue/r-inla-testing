n <- 100
y <- rnorm(n)
x <- rnorm(n)
xx <- rnorm(n)
xxx <- rev(xx)
idx <- 1:n
iidx <- n:1
formula <- y ~ -1 + f(x, model = "clinear", range = c(0, 1)) +
    f(xx, model = "mec") + f(xxx, copy = "xx")

r <- inla(formula,
          data = data.frame(y, x, xx, xxx, idx, iidx),
          control.family = list(hyper = list(prec = list(initial = 0, fixed = TRUE))),
          verbose = TRUE, ## keep=TRUE,
          inla.call = "inla.mkl.work",
          inla.arg = "-v -b -t1")
rr <- inla(formula,
          data = data.frame(y, x, xx, xxx, idx, iidx),
          control.family = list(hyper = list(prec = list(initial = 0, fixed = TRUE))),
          verbose = TRUE, ## keep=TRUE,
          inla.call = "inla.mkl.work",
          inla.arg = "-v -b -t1 -P")

