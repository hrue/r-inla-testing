set.seed(123)
n <- 10
y <- 1:n
x <- sin(y/n * 2 * pi)-1
xx <- cos(y)+1

A <- cbind(rep(1, n), x, xx)
AtA <- t(A) %*% A

Sys.setenv(INLA_DEBUG = "GMRFLib_preopt_init")
r <- inla(y ~ 1 + x + xx,
          family = "gaussian",
          control.family = list(
              hyper = list(
                  prec = list(initial = 0,
                              fixed = TRUE))), 
          control.fixed = list(prec.intercept = 1, prec = 1), 
          data = data.frame(y, x, xx),
          verbose = TRUE,
          inla.call = "inla.mkl.work",
          num.threads = "1:1",
          inla.mode = "experimental", 
          safe = FALSE)

eta=y/(n+1)
t(A) %*% eta
t(A) %*% A

