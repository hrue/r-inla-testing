INLA:::inla.my.update(b = T)

n <- 1000
rho <- 0.1
s <- 0.5
x <- scale(arima.sim(n, model = list(ar = rho)))
y <- x + rnorm(n, sd = s)

r <- inla(y ~ -1 + f(idx, model = "ar1", cyclic = T, 
                     hyper = list(prec = list(initial = 0,
                                              fixed = TRUE),
                                  rho = list(initial = inla.models()$latent$ar1$hyper$theta2$to.theta(rho), 
                                             fixed = TRUE))),
          data = data.frame(y, idx = 1:n), 
          family = "gaussian",
          control.family = list(hyper = list(prec = list(initial = log(1/s^2),
                                                         fixed = TRUE))),
          control.compute = list(control.gcpo = list(enable = TRUE,
                                                     num.level.sets = -1,
                                                     selection = -(1:n), 
                                                     verbose = !TRUE)),
          control.inla = list(int.strategy = "eb"),
          verbose = TRUE,
          safe = FALSE)
rr <- inla(y ~ -1 + f(idx, model = "ar1", cyclic = T, 
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
                                                     selection = -(1:n), 
                                                     verbose = !TRUE)),
          control.inla = list(int.strategy = "eb"),
          verbose = TRUE,
          safe = FALSE)

rr$gcpo

res <- numeric(n)
for(i in 1:n) {
    res[i] <- prod(r$gcpo$gcpo[rr$gcpo$groups[[i]]$idx])
}
plot(res, rr$gcpo$gcpo)
abline(a=0,b=1)
