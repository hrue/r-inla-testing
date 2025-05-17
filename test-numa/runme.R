n <- 10^3
x <- rnorm(n, sd = 0.2)
y <- rpois(n, exp(x))
idx <- 1:n

r <- inla(y ~ 1 + f(idx, model = "iid", vb.correct = 1:n),
          data = data.frame(y, idx = 1:n ),
          family = "poisson",
          verbose = TRUE,
          num.threads = "50:1")

