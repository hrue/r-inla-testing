n <- 15
y <- rnorm(n)

r <- inla(y ~ -1 + f(idx, model = "rw1", hyper = list(prec = list(initial = 0, fixed = TRUE)),
                     constr = FALSE),
          data = data.frame(y = y,
                            idx = 1:n),
          family = "stdnormal",
          keep = T)

