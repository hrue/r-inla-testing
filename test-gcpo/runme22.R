n <- 10
rho <- 0.9
s <- 100
x <- scale(arima.sim(n, model = list(ar = rho)))
y <- x + rnorm(n, sd = s)
r <- inla(y ~ -1 + f(idx, model = "ar1",
                     hyper = list(prec = list(initial = 0,
                                              fixed = TRUE),
                                  rho = list(initial = inla.models()$latent$ar1$hyper$theta2$to.theta(rho), 
                                             fixed = TRUE))),
          data = data.frame(y, idx = 1:n), 
          family = "gaussian",
          control.family = list(hyper = list(prec = list(initial = log(1/s^2),
                                                         fixed = TRUE))),
          control.compute = list(control.gcpo = list(enable = TRUE,
                                                     num.level.sets = 3,
                                                     ##selection = seq(1, n, by = 2), 
                                                     group.selection = seq(1, n, by = 2), 
                                                     verbose = !TRUE)),
          control.inla = list(int.strategy = "eb"),
          verbose = !TRUE,
          safe = FALSE, 
          num.threads = "1:1")
r$gcpo$groups
