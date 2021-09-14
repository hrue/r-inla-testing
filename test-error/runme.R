n <- 10
x <- 1:n
y <- x
r <- inla(y ~ -1 + x + f(x),
          data = data.frame(y, x, xx = x),
          keep = TRUE,
          verbose = TRUE)
