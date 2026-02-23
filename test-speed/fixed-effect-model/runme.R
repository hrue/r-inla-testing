n <- 10^5
m <- 10^2
x <- rnorm(n)
xx <- rnorm(n)
xxx <- rnorm(n)
xxxx <- rnorm(n)
y <- rnorm(n)

x[1:m] <- 1
xx[1:m] <- 1
xxx[1:m] <- 1
xxxx[1:m] <- 1

r <- inla(y ~ x * xx * xxx * xxxx,
          data = data.frame(y, x, xx, xxx, xxxx),
          family = "normal",
          keep = TRUE,
          verbose = TRUE,
          inla.call = "")


