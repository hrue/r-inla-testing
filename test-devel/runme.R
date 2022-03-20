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
hist(xx, prob = TRUE, lwd = 3, n = 90)
lines(inla.smarginal(r$marginals.fitted.values[[1]]),
      lwd = 2, col = "blue")

print("lambda")
print(c(mean(xx), sd(xx)))
print(r$summary.fitted.values)

print("log(lambda")
print(c(mean(log(xx)), sd(log(xx))))
print(r$summary.linear.predictor)


