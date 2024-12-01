INLA:::inla.my.update(b = T)
inla.setOption(enable.experimental.features = TRUE)

n <- 30
y <- scale(rnorm(n))

r <- inla(y ~ -1 + f(idx, 
                     hyper = list(prec = list(prior = "pc.prec",
                                              param = c(1000, 0.01),
                                              same.as = "Log precision for the Gaussian observations"))), 
          data = data.frame(y, idx = 1:n),
          control.inla = list(int.strategy = "eb"), 
          verbose = TRUE)
plot(r$summary.random$idx$mean, r$summary.linear.predictor$mean)
abline(a=0,b=1)
