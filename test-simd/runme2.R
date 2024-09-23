n <- 10^6
size <- sample(1:5, n, replace = TRUE)
y <- rbinom(n, size = size, prob = 0.1)
off <- rnorm(n, sd = 0.1)
off[abs(off) < 0.05] <- 0

rr <- inla(y ~ 1 + offset(off) + f(idx),
           data = data.frame(y, idx = rep(1, n), size, off),
           family = "binomial",
           Ntrials = size, 
           control.compute = list(return.marginals = !FALSE), 
           verbose = T)
r <- inla(y ~ 1 + offset(off) + f(idx),
          data = data.frame(y, idx = rep(1, n), size, off),
          family = "binomial",
          Ntrials = size, 
          verbose = TRUE,
          control.compute = list(return.marginals = !FALSE), 
          inla.call = "inla.mkl.work")
rr$cpu.used
r$cpu.used
r$mlik - rr$mlik
print(max(abs(r$summary.linear.predictor - rr$summary.linear.predictor)))
print(max(abs(r$summary.fitted.values - rr$summary.fitted.values)))

