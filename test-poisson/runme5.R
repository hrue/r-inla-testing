n <- 1000
x <- rnorm(n, sd = 1)
off <- rnorm(n, sd = 0.1)
eta <- 2 + 0.3 * x + off
lam <- exp(eta)
y <- rpois(n, lambda = lam)

inla.setOption(num.threads = "1:1",
               safe = FALSE,
               keep = F, 
               verbose = !TRUE)

r <- inla(y ~ 1 + x + offset(off),
          family = "poisson",
          control.compute = list(cpo = T),
          control.inla = list(tolerance.step = 1e-15), 
          data = data.frame(y, x, off))
rr <- inla(y ~ 1 + x + offset(off),
           family = "npoisson",
           control.compute = list(cpo = T), 
           control.inla = list(tolerance.step = 1e-15), 
           data = data.frame(y, x, off),
           inla.call = "inla.mkl.work")

r$logfile[grep("stage1", r$logfile)]
rr$logfile[grep("stage1", rr$logfile)]

r$summary.fixed - rr$summary.fixed
mean(abs(r$cpo$pit - rr$cpo$pit))

r$mlik - rr$mlik
