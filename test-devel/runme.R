inla.setOption(inla.mode = "experimental")
inla.setOption(inla.call = "inla.mkl")
inla.setOption(inla.call = "inla.mkl.work")

y <- 3
r <- inla(y ~ 1,
          data = data.frame(y = y),
          family = "poisson", 
          control.fixed = list(prec.intercept = 1), 
          control.predictor = list(compute = TRUE),
          control.compute = list(return.marginals.predictor = TRUE),
          verbose = TRUE)

rr <- inla(y ~ 1,
          data = data.frame(y = y),
          family = "poisson", 
          control.fixed = list(prec.intercept = 1), 
          control.predictor = list(compute = TRUE),
          control.compute = list(return.marginals.predictor = TRUE),
          inla.call = "inla.mkl",
          inla.mode = "classic",
          control.inla = list(strategy = "laplace"), 
         verbose = TRUE)

if (!exists("xx")) {
library(runjags)
xx <- combine.mcmc(run.jags("runme.jags",
                            data = list(y = y),
                            monitor = c("lambda"),
                            burnin = 10,
                            sample = 100000))
}

par(mfrow = c(1, 2))
hist(log(xx), prob = TRUE, lwd = 3, n = 90)
lines(inla.smarginal(r$marginals.linear.predictor[[1]]),
      lwd = 2, col = "blue")
lines(inla.smarginal(rr$marginals.linear.predictor[[1]]),
      lwd = 2, col = "red")
hist(xx, prob = TRUE, lwd = 3, n = 90)
lines(inla.smarginal(r$marginals.fitted.values[[1]]),
      lwd = 2, col = "blue")
lines(inla.smarginal(rr$marginals.fitted.values[[1]]),
      lwd = 2, col = "red")

print("lambda")
print(c(mean(xx), sd(xx)))
print(r$summary.fitted.values)
print(rr$summary.fitted.values)

print("log(lambda")
print(c(mean(log(xx)), sd(log(xx))))
print(r$summary.linear.predictor)
print(rr$summary.linear.predictor)
