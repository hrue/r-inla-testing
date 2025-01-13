n <- 100
x <- rnorm(n)
eta <- 1 + 2*x
eta.d <- c(0, diff(eta))
s <- 0.01
y <- rnorm(n, mean = eta + 0.1 * eta.d, sd = s)

formula <- Y ~ -1 + intercept + xx +
    f(eta, w, model = "iid",
      hyper = list(prec = list(initial = -15, fixed = TRUE))) +
    f(eta.1, copy = "eta",
      hyper = list(beta = list(initial = 0, param = c(0, 100), fixed = FALSE))) +
    f(eta.2, w2, copy = "eta",  same.as = "eta.1")

data <- list(
    Y = cbind(c(rep(0, n), rep(NA, n)), 
              c(rep(NA, n), y)), 
    intercept = c(rep(1, n), rep(NA, n)), 
    xx = c(x, rep(NA, n)), 
    eta = c(1:n, 1:n), 
    w = c(rep(-1, n), rep(1, n)), 
    eta.1 = c(rep(NA, n), 1:n), 
    eta.2 = c(rep(NA, n), NA, 1:(n-1)),
    w2 = c(rep(0, n), rep(-1, n)))

r <- inla(formula,
          data = data,
          family = rep("gaussian", 2),
          control.family = list(
              list(hyper = list(prec = list(initial = 15, fixed = TRUE))),
              list(hyper = list(prec = list(initial = log(1/s^2), fixed = TRUE)))),
          verbose = TRUE)
summary(r)
