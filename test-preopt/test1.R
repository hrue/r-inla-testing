n <- 25
x <- rnorm(n)
y <- x + rnorm(n, sd = 0.1)

r <- inla(y ~ -1 + x,
          data = data.frame(y, x),
          verbose = TRUE,
          ##keep = TRUE,
          num.threads = "1:1",
          control.predictor = list(compute = TRUE), 
          control.inla = list(int.strategy = "eb"), 
          inla.call = "inla.mkl.work")

m <- r$marginals.fixed[[1]]
plot(m[, "x"], m[, "y"] )
