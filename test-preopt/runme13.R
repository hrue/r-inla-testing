n <- 10
y <- rnorm(n)
Am <- INLA:::inla.rw1(n)
r <- inla(y ~ 1 + x + f(idx, model = "rw1"),
          data = data.frame(y = y, x = 1:n, idx = 1:n),
          control.predictor = list(A = Am, compute = T), 
          inla.call = "inla.mkl",
          num.threads = "1:1", 
          inla.mode = "experimental")
rr <- inla(y ~ 1 + x + f(idx, model = "rw1"),
          data = data.frame(y = y, x = 1:n, idx = 1:n),
          verbose = TRUE,
          control.predictor = list(A = Am, compute = T), 
          inla.call = "inla.mkl.work",
          num.threads = "1:1", 
          inla.mode = "experimental")

print(r$mlik - rr$mlik )
print(mean(abs(r$summary.linear.predictor$mean - rr$summary.linear.predictor$mean)))


r <- inla(y ~ 1 + x + f(idx, model = "rw1"),
          data = data.frame(y = y, x = 1:n, idx = 1:n),
          control.predictor = list(compute = T), 
          inla.call = "inla.mkl",
          num.threads = "1:1", 
          inla.mode = "experimental")
rr <- inla(y ~ 1 + x + f(idx, model = "rw1"),
          data = data.frame(y = y, x = 1:n, idx = 1:n),
          control.predictor = list(compute = T), 
          inla.call = "inla.mkl.work",
          num.threads = "1:1", 
          inla.mode = "experimental")

print(r$mlik - rr$mlik )
print(mean(abs(r$summary.linear.predictor$mean - rr$summary.linear.predictor$mean)))
