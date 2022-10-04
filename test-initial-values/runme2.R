set.seed(123)
n <- 10
y <- 1:n
x <- sin(y/n * 2 * pi)-1

A <- cbind(rep(1, n), x)

Sys.setenv(INLA_DEBUG = "GMRFLib_preopt_init")
r <- inla(y ~ 0 + intercept + 
              f(ix, model = "iid", hyper = list(prec = list(initial = 0, fixed = TRUE))), 
          family = "gaussian",
          control.family = list(
              hyper = list(
                  prec = list(initial = 0,
                              fixed = TRUE))), 
          control.fixed = list(prec.intercept = 1, prec = 1), 
          data = list(A = A, intercept = c(1, NA), ix = c(NA, 1), y = y),
          control.predictor = list(A = A), 
          verbose = TRUE,
          inla.call = "inla.mkl.work",
          num.threads = "1:1",
          inla.mode = "experimental", 
          safe = FALSE)

eta=y/(n+1)
AtA <- t(A) %*% A
t(A) %*% eta
t(A) %*% A

