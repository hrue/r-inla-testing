N <- 100000
n <- 1:N
y <- rnorm(N, sd = sqrt(1/n))
idx <- 1:N

r <- inla(y ~ 1, 
          scale = n, 
          keep = TRUE, 
          data = data.frame(y, idx, n))
summary(r)

