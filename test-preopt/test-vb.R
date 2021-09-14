n <- 20
x <- rnorm(n, sd = 0.1)
s <- 0.1
eta <- -2 + x
eta.s <- eta + rnorm(m, sd = 0.1)
y <- rpois(n, exp(eta.s))


inla.setOption(num.threads = "1:1")
inla.setOption(inla.call = "inla.mkl.work")
##inla.setOption(inla.call = "inla.valgrind")
r <- inla(y ~ 1 + x, 
          data = list(y = y, x = x), 
          family = "nbinomial", 
          control.predictor = list(compute = TRUE), 
          control.fixed = list(prec.intercept = 0.01), 
          control.inla = list(int.strategy = "eb", strategy = "gaussian", 
                              control.twostage = list(stage1only = TRUE),
                              control.vb = list(enable = FALSE, verbose = TRUE)), 
          twostage = TRUE, 
          verbose = FALSE)
                              
rr <- inla(y ~ 1 + x, 
          data = list(y = y, x = x), 
          family = "nbinomial", 
          ##control.family = list(hyper = list(size = list(param = 17))), 
          control.predictor = list(compute = TRUE), 
          control.fixed = list(prec.intercept = 0.01), 
          control.inla = list(int.strategy = "eb", strategy = "gaussian", 
                              control.twostage = list(stage1only = TRUE),
                              control.vb = list(enable = TRUE, verbose = T)),
          twostage = TRUE,
          verbose = FALSE)
                              

rrr <- inla(y ~ 1 + x, 
           data = list(y = y, x = x), 
           ##family = "poisson", 
           family = "nbinomial", 
           ##control.family = list(hyper = list(size = list(param = 17))), 
           control.predictor = list(compute = TRUE), 
           control.fixed = list(prec.intercept = 0.01), 
           control.inla = list(int.strategy = "eb", strategy = "laplace"), 
           twostage = FALSE)

rbind(round(dig=3, r$summary.fixed$mean), 
      round(dig=3, rr$summary.fixed$mean), 
      round(dig=3, rrr$summary.fixed$mean))

