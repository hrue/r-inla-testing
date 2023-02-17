set.seed(1234)
n <- 10
A <- diag(n)
x <- rnorm(n, sd = 0.2)
eta <-  -1 + x
y <- rpois(n, exp(eta))

r <- inla(y  ~ 1 + x,
          data = data.frame(y, x),
          family = "poisson",
          control.compute = list(config = TRUE),
          control.predictor = list(A = A), 
          control.inla = list(control.vb = list(enable = FALSE)))
           
