INLA:::inla.my.update(b = T)

n <- 100
rho <- 0.8
s <- 100
x <- scale(arima.sim(n, model = list(ar = rho)))
y <- x + rnorm(n, sd = s)

type.cv <- "single"
r <- inla(y ~ -1 + f(idx, model = "ar1", cyclic = FALSE, 
                     hyper = list(prec = list(initial = 0,
                                              fixed = TRUE),
                                  rho = list(initial = inla.models()$latent$ar1$hyper$theta2$to.theta(rho), 
                                             fixed = TRUE))),
          data = data.frame(y, idx = 1:n), 
          family = "gaussian",
          control.family = list(hyper = list(prec = list(initial = log(1/s^2),
                                                         fixed = TRUE))),
          control.compute = list(control.gcpo = list(enable = TRUE,
                                                     num.level.sets = 2,
                                                     type.cv = type.cv, 
                                                     verbose = TRUE)),
          control.inla = list(int.strategy = "eb"),
          num.threads = "1:1", 
          verbose = !TRUE,
          safe = FALSE)

m <- 50
r$gcpo$groups[[m]]
r$gcpo$gcpo[m]

g <- inla.group.cv(r, groups = r$gcpo$groups, type.cv = type.cv)
g$cv[m]

rr <- inla(y ~ -1 + f(idx, model = "ar1", cyclic = FALSE, 
                     hyper = list(prec = list(initial = 0,
                                              fixed = TRUE),
                                  rho = list(initial = inla.models()$latent$ar1$hyper$theta2$to.theta(rho), 
                                             fixed = TRUE))),
          data = data.frame(y, idx = 1:n), 
          family = "gaussian",
          control.family = list(hyper = list(prec = list(initial = log(1/s^2),
                                                         fixed = TRUE))),
          control.compute = list(control.gcpo = list(enable = TRUE,
                                                     type.cv = type.cv, 
                                                     num.level.sets = 2,
                                                     groups = r$gcpo$groups, 
                                                     verbose = TRUE)),
          control.inla = list(int.strategy = "eb"),
          num.threads = "1:1", 
          verbose = !TRUE,
          safe = FALSE)
rr$gcpo$gcpo[m]

