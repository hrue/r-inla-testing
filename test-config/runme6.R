INLA:::inla.my.update()
inla.setOption(inla.call = "inla.mkl.work")

set.seed(1234)
m <- 10
n <- 10
A <- diag(1, m, n)
x <- rnorm(n, sd = 0.2)
eta <-  -1 + x
y <- rpois(m, exp(A %*% eta))

r <- inla(y  ~ 1 + x,
          data = list(y = y, x = x, A = A),
          family = "poisson",
          control.compute = list(config = TRUE),
          control.predictor = list(A = A, compute = TRUE), 
          control.inla = list(control.vb = list(enable = FALSE)))
rr <- inla(y  ~ 1 + x,
          data = list(y = y, x = x, A = A),
          family = "poisson",
          control.compute = list(config = TRUE),
          control.predictor = list(compute = TRUE), 
          control.inla = list(control.vb = list(enable = FALSE)))
           
