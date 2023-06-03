n <- 40000
x <- rnorm(n, sd = 1)
off <- rnorm(n, sd = 0.001)
off[] <- 0
eta <- 0 + 0.3 * x + off
lam <- exp(eta)
y <- rpois(n, lambda = lam)

inla.setOption(num.threads = "1:1",
               safe = FALSE,
               keep = F, 
               verbose = !TRUE)

r <- inla(y ~ 1 + x + offset(off),
          family = "poisson",
          control.compute = list(cpo = T), 
          data = data.frame(y, x, off))
rr <- inla(y ~ 1 + x + offset(off),
           family = "poisson",
           control.compute = list(cpo = T), 
           data = data.frame(y, x, off),
           inla.call = "inla.mkl.work")

r$logfile[grep("stage1", r$logfile)]
rr$logfile[grep("stage1", rr$logfile)]

r$summary.fixed - rr$summary.fixed
mean(abs(r$cpo$pit - rr$cpo$pit))
