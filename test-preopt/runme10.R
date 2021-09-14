INLA:::inla.my.update()
INLA:::inla.my.update()

n <- 100
x <- rnorm(n)
xx <- rnorm(n)
s <- 0.1
y <- 1 + x + xx + rnorm(n, sd = s)

r <- inla(y ~ 1 + x + f(inla.group(xx), model = "rw2",
                        scale.model = TRUE),
          data = data.frame(y, x, xx),
          verbose = TRUE,
          inla.call = "inla.mkl.work",
          inla.arg = "-v -b -t4:1 -P")
