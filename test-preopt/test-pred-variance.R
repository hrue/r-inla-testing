n <- 10
m <- n + 1
x <- rnorm(n)
s <- 0.1
eta <- 1 + x
A <- matrix(runif(m*n), m, n)
eta.s <- A %*% eta
y <- eta.s + rnorm(m, sd = 0.1)

inla.setOption(inla.call = "inla.valgrind")
inla.setOption(inla.call = "inla.mkl.work")
r <- inla(y ~ 1 + x, 
          data = list(y = y, x = x, A = A), 
          control.family = list(
              hyper = list(prec = list(initial = log(1/s^2),
                                       param = c(100000, 1000), 
                                       fixed = FALSE))),
          control.predictor = list(compute = TRUE, A = A), 
          control.inla = list(int.strategy = "eb"))

rr <- inla(y ~ 1 + x, 
           data = list(y = y, x = x, A = A), 
           control.family = list(
               hyper = list(prec = list(initial = log(1/s^2),
                                        param = c(100000, 1000), 
                                        fixed = FALSE))),
           control.inla = list(int.strategy = "eb"),
           control.predictor = list(compute = TRUE, A = A), 
           control.mode = list(result = r, restart = FALSE), 
           twostage = TRUE,
           verbose = FALSE)

round(dig = 4, cbind(c(1:m, 1:n),
                     1:(m+n), 
                     r$summary.linear.predictor[, "mean"],
                     r$summary.linear.predictor[, "sd"]))
