n <- 100
y <- rep(NA, n)
idx <- 1:n

r <- inla(y ~ -1 + f(idx, w, model = "rw1",
                     hyper = list(prec = list(fixed = T))), 
          data = data.frame(y, idx, w = 1:n), 
          family = "stdnormal",
          verbose = TRUE)
