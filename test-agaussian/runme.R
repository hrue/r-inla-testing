##INLA:::inla.my.update()
inla.setOption(inla.call = "inla.mkl.work")

n <- 10
y <- rnorm(n)
Y <- inla.agaussian(y)

r <- inla(Y ~ 1,
          data = list(Y = Y),
          family = "agaussian",
          verbose = TRUE)

rr <- inla(y ~ 1,
           data = data.frame(y), 
           family = "gaussian",
           verbose = TRUE)

plot(r$internal.marginals.hyperpar[[1]], pch = 19, main = "prec")
lines(rr$internal.marginals.hyperpar[[1]], lwd = 3)

dev.new()
plot(r$marginals.fixed$'(Intercept)', pch = 19, main = "intercept")
lines(rr$marginals.fixed$'(Intercept)', lwd = 3)
