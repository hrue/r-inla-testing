## do linear regression where data are subject to left censoring
n <- 300
x <- rnorm(n)
eta <- 1 + 0.2 * x
s <- 0.1
y <- eta + rnorm(n, sd = s)
L <- 1
y <- pmax(L, y)

idx <- which(y <= L)
e <- rep(1, n)
e[idx] <- 2
YY <- inla.surv(time = exp(y), event = e)

r <- inla(YY ~ 1 + x,
          data = list(YY = YY, x = x),
          family = "lognormalsurv")
summary(r)



