n <- 10^5
E <- runif(n)
y <- rpois(n, E*exp(0))
off <- rnorm(n, sd = 0.1)
off[abs(off) < 0.05] <- 0

r <- inla(y ~ 1 + offset(off) + f(idx),
          data = data.frame(y, off, idx = rep(1, n), E),
          family = "poisson",
          E = E, 
          verbose = TRUE)
rr <- inla(y ~ 1 + offset(off) + f(idx),
           data = data.frame(y, off, idx = rep(1, n), E),
           family = "poisson",
           E = E, 
           verbose = TRUE,
           inla.call = "inla.mkl.work")
