n <- 300
nc <- 33
A <- matrix(rnorm(n*nc), nc, n)
e <- rnorm(nc)
constr <- list(A = A, e = e)
lc <- inla.make.lincombs(idx = matrix(runif(n^2), n, n))

y <- rpois(n, exp(1))
inla.setOption(num.threads = 8)

r <- inla(y ~ 1 + f(idx, model = "iid", extraconstr = constr), 
          data = list(y = y, constr = constr, n = n, idx = 1:n),
          family = "poisson",
          verbose = TRUE,
          lincomb = lc, 
          control.inla = list(tolerance = 1E-9,
                              lincomb.derived.correlation.matrix = TRUE), 
          control.compute = list(control.gcpo = list(enable = TRUE,
                                                     num.level.sets = 5),
                                 waic = TRUE))

rr <- inla(y ~ 1 + f(idx,  model = "iid", extraconstr = constr), 
           data = list(y = y, constr = constr, n = n, idx = 1:n),
           family = "poisson",
           verbose = TRUE,
           lincomb = lc, 
           control.inla = list(tolerance = 1E-9,
                               lincomb.derived.correlation.matrix = TRUE), 
           control.compute = list(control.gcpo = list(enable = TRUE,
                                                      num.level.sets = 5),
                                  waic = TRUE),
           inla.call = "inla.mkl.work")

r$mlik - rr$mlik
max(abs(r$gcpo$gcpo - rr$gcpo$gcpo))
max(abs(r$mode$x - rr$mode$x))
max(abs(r$summary.linear.predictor$mean - rr$summary.linear.predictor$mean))
max(abs(r$summary.lincomb.derived$mean - rr$summary.lincomb.derived$mean))
max(abs(r$summary.lincomb.derived$sd/rr$summary.lincomb.derived$sd-1))
max(abs(c(r$misc$lincomb.derived.correlation.matrix) -
        c(rr$misc$lincomb.derived.correlation.matrix)))
max(abs(r$waic$local.waic - rr$waic$local.waic))
