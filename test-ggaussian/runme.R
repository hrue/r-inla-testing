INLA:::inla.my.update(b = T)
n <- 10000
x <- rnorm(n)
xx <- rnorm(n)
eta <-  0.1 + 1.1 * x + 2.2 * xx
s <- runif(n)

z <- rnorm(n) 
zz <- rnorm(n)
eta.prec <- 1 + 0.55 * z + 1.1 * zz

y <- eta + 1/sqrt(s * exp(eta.prec)) * rnorm(n)
Y <- inla.mdata(y, s, 1, z, zz)
r <- inla(Y ~ 1 + x + xx,
          data = list(Y = Y, x = x, xx = xx, z = z, zz = zz, s = s),
          family = "ggaussian",
          verbose = T)
summary(r)

Y <- inla.mdata(y, s)
rr <- inla(Y ~ 1 + x + xx,
           data = list(Y = Y, x = x, xx = xx, z = z, zz = zz, s = s),
           family = "ggaussian",
           verbose = !TRUE,
           safe = FALSE)
summary(rr)

