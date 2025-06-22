n <- 10000
y <- rnorm(n)
nc <- 4
constr <- list(A = matrix(rnorm(n*nc), nrow = nc, ncol = n),
               e = rnorm(nc))

r <- inla(y ~ -1 + f(idx, model = "iid",
                     constr = FALSE,
                     extraconstr = constr), 
          data = data.frame(y, idx = 1:n),
          family = "stdnormal",
          control.compute = list(control.gcpo = list(enable = TRUE)),
          keep = TRUE,
          verbose = TRUE)

