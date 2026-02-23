n <- 10^5
x <- rnorm(n, sd = 0.2)
prec <- 100
s <- sqrt(1/prec)
a <- 0.1
b <- 0.2
y <- a + b * x + s * rnorm(n)

y.left <- 0.1
y[y <= y.left] <- y.left
event <- rep(1, n)
event[y <= y.left] <- 2
Y <- inla.surv(exp(y), event)
r <- inla(Y ~ 1 + x,
          data = list(Y = Y, event = event, x = x),
          family = "lognormalsurv")
summary(r)

cbind(estimated.values =  c(r$summary.fixed$mean, r$summary.hyperpar$mean), 
      true.values = c(a, b, prec))
