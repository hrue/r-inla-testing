set.seed(123)
n <- 50
x <- sin((1:n) / (2 * pi))
s <- 0.1
y <- x + rnorm(n, sd = s)

Y <- c(y, y)
idx.1 <- c(1:n, rep(NA, n))
idx.2 <- c(rep(NA, n), 1:n)

r <- inla(Y ~
              f(idx.1, model = "rw2",  scale.model = TRUE) +
              f(idx.2, model = "rw2",  scale.model = TRUE),
          data = list(Y = Y, idx.1 = idx.1, idx.2 = idx.2),
          family = "gaussian",
          control.family = list(hyper = list(prec = list(initial = log(1/s^2),
                                                         fixed = TRUE))), 
          inla.call = "inla.mkl.work",
          verbose = TRUE,
          safe = FALSE)


              

