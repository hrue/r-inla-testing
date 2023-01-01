inla.setOption(safe = F)
n <- 1000000
x <- rnorm(n)
eta <- 0 + 0.3 * x
y <- rpois(n, exp(eta))

if (F)r <- inla(y ~ 1 + x,
          family = "poisson",
          data = data.frame(y, x))
if (F)rr <- inla(y ~ 1 + x,
          family = "poisson",
          data = data.frame(y, x),
          inla.call = "inla.mkl.work")
if (F) r <- inla(y ~ 1 + x,
          family = "poisson",
          data = data.frame(y, x))
rr <- inla(y ~ 1 + x,
          family = "poisson",
          data = data.frame(y, x),
          inla.call = "inla.mkl.work",
          verbose = T)
##r$mlik - rr$mlik
##r$logfile[grep("stage1", r$logfile)]
##rr$logfile[grep("stage1", rr$logfile)]


