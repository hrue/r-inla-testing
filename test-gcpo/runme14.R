## build groups using AR1

INLA:::inla.my.update(b = T)

n <- 30
s <- 0.1
xx <- rnorm(n)
yy <- rnorm(n)
y <- 1 + xx + yy + rnorm(n, sd = s)
r <- inla(y ~ 1 + xx + yy, 
          data = data.frame(y, xx, yy), 
          family = "gaussian",
          control.family = list(hyper = list(prec = list(initial = log(1/s^2),
                                                         fixed = !TRUE))),
          inla.mode = "experimental",
          control.compute = list(config = TRUE,
                                 cpo = !TRUE, 
                                 control.gcpo = list(enable = !TRUE,
                                                     num.level.sets = -1,
                                                     strategy = "posterior",
                                                     verbose = TRUE)),
          ##control.inla = list(int.strategy = "eb"),
          verbose = TRUE,
          safe = FALSE, 
          num.threads = "1:1", 
          inla.call = "inla.mkl.work")
r$gcpo$groups
