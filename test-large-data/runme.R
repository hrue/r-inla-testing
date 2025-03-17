n <- 1024^3
x <- rnorm(n)
xx <- rnorm(n)
y <- 1 + x + xx + rnorm(n)

r <- inla(y ~ 1 + x + xx,
          data = data.frame(y, x, xx),
          family = "stdnormal",
          num.threads = "1:20", 
          verbose = TRUE)

