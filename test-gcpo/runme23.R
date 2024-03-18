n <- 20
rho <- 0.5
s <- 5
x <- scale(arima.sim(n, model = list(ar = rho)))
y <- x + rnorm(n, sd = s)
nt <- "1:1"

INLA:::inla.my.update()
rr <- inla(y ~ 1 + f(idx, model = "ar1",
                     hyper = list(prec = list(initial = 0,
                                              fixed = TRUE),
                                  rho = list(initial = inla.models()$latent$ar1$hyper$theta2$to.theta(rho), 
                                             fixed = TRUE))),
           data = data.frame(y, idx = 1:n), 
           family = "gaussian",
           control.family = list(hyper = list(prec = list(initial = log(1/s^2),
                                                          fixed = TRUE))),
           control.compute = list(cpo = T,
                                  control.gcpo = list(enable = TRUE,
                                                      num.level.sets = 2,
                                                      size.max = 32, 
                                                      weights = seq(0.1, 1, len = n), 
                                                      selection = seq(1, n), 
                                                      verbose = !TRUE)),
           ##control.inla = list(int.strategy = "eb"),
           verbose = TRUE,
           safe = FALSE, keep = !T, 
           num.threads = nt, inla.call = "inla.mkl.work")

rr$gcpo$groups
