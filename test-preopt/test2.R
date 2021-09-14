n <- 5
y <- rnorm(n)
inla.setOption(inla.call = "inla.mkl.work")
r <- inla(y ~ -1 +
              f(idx, model = "rw2", scale.model = FALSE) +
              f(iidx, model = "rw2", scale.model = TRUE),
          data = data.frame(y, idx = 1:n, iidx = n:1),
          verbose = TRUE,
          control.family = list(hyper = list(prec = list(initial = 0, fixed = TRUE))))

