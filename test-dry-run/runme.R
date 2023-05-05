n <- 100
x <- rnorm(n)
u <- rnorm(n)
idx <- 1:n
eta <- 1 + 0.2 * x + 0.1 * u
y <- rpois(n, exp(eta))

r <- inla.dryrun(y ~ 1 + x + f(idx),
                 family = "poisson",
                 data = data.frame(y = y, x = x, idx),
                 verbose = T, debug = TRUE,  inla.call = "", 
                 control.mode = list(fixed = TRUE, restart = FALSE),
                 control.inla = list(control.vb = list(enable = FALSE)))
