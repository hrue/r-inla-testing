n <- 10
x <- rnorm(n)
eta <- 1 + x
y <- eta + rnorm(n, sd = 0.1)

off.1 <- 1:10
off.2 <- 101:110
off.3 <- 1001:1010

r <- inla(y ~ 1 + x + offset(off.1) + offset(off.2), offset = off.3,
          data = data.frame(y, x, off.1, off.2, off.3))
cbind(r$offset.linear.predictor, off.1 + off.2 + off.3)
