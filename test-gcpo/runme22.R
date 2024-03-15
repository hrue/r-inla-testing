n <- 10000
rho <- 0.9
s <- 1000
x <- scale(arima.sim(n, model = list(ar = rho)))
xx <- rnorm(n)
y <- x + rnorm(n, sd = s)

r <- inla(y ~ 1 + xx + f(idx, model = "ar1",
                     hyper = list(prec = list(initial = 0,
                                              fixed = !TRUE),
                                  rho = list(initial = inla.models()$latent$ar1$hyper$theta2$to.theta(rho), 
                                             fixed = !TRUE))),
          data = data.frame(y, idx = 1:n, xx), 
          family = "gaussian",
          control.family = list(hyper = list(prec = list(initial = log(1/s^2),
                                                         fixed = TRUE))),
          control.compute = list(control.gcpo = list(enable = TRUE,
                                                     num.level.sets = 5,
                                                     ##selection = seq(1, n, by = 2), 
                                                     ##group.selection = seq(1, n, by = 2), 
                                                     verbose = !TRUE)),
          ##control.inla = list(int.strategy = "eb"),
          verbose = TRUE, 
          safe = FALSE, 
          num.threads = "4:1")

INLA:::inla.my.update()
rr <- inla(y ~ 1 + xx + f(idx, model = "ar1",
                     hyper = list(prec = list(initial = 0,
                                              fixed = !TRUE),
                                  rho = list(initial = inla.models()$latent$ar1$hyper$theta2$to.theta(rho), 
                                             fixed = !TRUE))),
          data = data.frame(y, idx = 1:n, xx), 
          family = "gaussian",
          control.family = list(hyper = list(prec = list(initial = log(1/s^2),
                                                         fixed = TRUE))),
          control.compute = list(control.gcpo = list(enable = TRUE,
                                                     num.level.sets = 5,
                                                     ##selection = seq(1, n, by = 2), 
                                                     ##group.selection = seq(1, n, by = 2), 
                                                     verbose = !TRUE)),
          ##control.inla = list(int.strategy = "eb"),
          verbose = TRUE,
          safe = FALSE, keep = !T, 
          num.threads = "4:1", inla.call = "inla.mkl.work")

r$cpu.intern
rr$cpu.intern

print(max(abs(r$gcpo$gcpo - rr$gcpo$gcpo)))
