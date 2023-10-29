n <- 20
x <- rnorm(n)
s <- 1
y <- 1 + x + rnorm(n, sd = s)

INLA:::inla.my.update()
inla.setOption(num.threads = "1:1", smtp = "taucs", safe = FALSE)

r <- inla(y ~ 1+ x, 
          data = data.frame(y, x, idx = 1:n),
          family = "normal",
          control.family = list(hyper = list(prec = list(initial = log(1/s^2), fixed = !TRUE, param = c(100, 100)))), 
          ##control.expert = list(disable.gaussian.check = TRUE), 
          control.fixed = list(prec.intercept = 1, prec = 1),
          inla.call = "inla.mkl.work")

Sys.setenv(INLA_NEW_TEST = 1)
rr <- inla(y ~ 1+ x, 
           data = data.frame(y, x, idx = 1:n),
           family = "normal",
           control.family = list(hyper = list(prec = list(initial = log(1/s^2), fixed = !TRUE, param = c(100, 100)))), 
           control.fixed = list(prec.intercept = 1, prec = 1), 
           verbose=TRUE, 
           control.expert = list(disable.gaussian.check = TRUE), 
           inla.call = "inla.mkl.work")

k <- 1
plot(inla.smarginal(r$internal.marginals.hyperpar[[k]]), lwd = 7, col = "blue", type = "l")
lines(inla.smarginal(rr$internal.marginals.hyperpar[[k]]), lwd = 3, col = "yellow")
abline(v=r$mode$theta[k], col="blue",lwd=3)
abline(v=rr$mode$theta[k], col="yellow",lwd=3)
