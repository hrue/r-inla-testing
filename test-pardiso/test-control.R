inla.setOption(inla.call = 'inla.mkl.work')
n <- 10
x <- rnorm(n)
y <- 1 + x + rnorm(n, sd = 0.1)

r <- inla(y ~ 1 +x,
          data = data.frame(y, x),
          control.pardiso = list(verbose = T,
                                 debug = T,
                                 nrhs = 2,
                                 parallel.reordering = TRUE),
          verbose = TRUE)
