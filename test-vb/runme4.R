##INLA:::inla.my.update(b = T)
##set.seed(123)
n <- 20
x <- rnorm(n, sd = 0.1)
eta <- 1+x
y <- rpois(n, exp(eta))
family <- "poisson"
r <- inla(y ~ 1+f(idx, model = "iid", vb.correct = c(1, 3, 20)), 
          data = data.frame(y, idx = 1:n),
          family = family, 
          control.fixed = list(prec = 0.1, prec.intercept = 0.1), 
          control.inla = list(control.vb = list(enable = TRUE,  strategy = "mean")), 
          control.predictor = list(compute = TRUE), 
          verbose = TRUE, keep = TRUE, 
          inla.call = "inla.mkl.work",
          num.threads = "1:1", 
          inla.mode = "experimental")

