n <- 5
s <- 1:n
y <- rnorm(n)
yy <- rnorm(n, sd = sqrt(1/s))

Y1 <- unlist(inla.agaussian(y))
Y2 <- unlist(inla.agaussian(y, s))
Y12 <- inla.mdata(rbind(Y1, Y2))


Y <- inla.agaussian(rbind(y, yy),
                    rbind(rep(1, n), s))
r <- inla(Y ~ 1,
          data = list(Y = Y),
          family = "agaussian",
          control.compute = list(cpo = TRUE, dic = TRUE), 
          verbose = TRUE)

rr <- inla(yyy ~ 1,
           data = data.frame(yyy = c(y, yy)),
           scale = c(rep(1, n), s), 
           control.compute = list(cpo = TRUE, dic = TRUE), 
           family = "gaussian",
           verbose = TRUE)

plot(r$internal.marginals.hyperpar[[1]], pch = 19, main = "prec")
lines(rr$internal.marginals.hyperpar[[1]], lwd = 3)

dev.new()
plot(r$marginals.fixed$'(Intercept)', pch = 19, main = "intercept")
lines(rr$marginals.fixed$'(Intercept)', lwd = 3)
