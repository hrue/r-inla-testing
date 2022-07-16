##INLA:::inla.my.update(b = T)
##set.seed(123)
n <- 10
x <- rnorm(n, sd = 0.1)
eta <- 1+x
y <- rpois(n, exp(eta))
family <- "poisson"
r <- inla(y ~ 1+x, data = data.frame(y, x),
          family = family, 
          control.fixed = list(prec = 0.1, prec.intercept = 0.1), 
          control.inla = list(control.vb = list(enable = TRUE,  strategy = "mean")), 
          control.predictor = list(compute = TRUE), 
          verbose = TRUE,
          inla.call = "inla.mkl.work",
          num.threads = "1:1", 
          inla.mode = "experimental")

