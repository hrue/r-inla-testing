N <- 100
n <- (1:N)
y <- rnorm(N, sd = sqrt(1/n))
idx <- 1:N

r <- inla(y ~ as.factor(idx) -1, 
          scale = n, 
          control.fixed = list(prec = 0), 
          control.family = list(hyper = list(
                                    prec = list(initial = 0,
                                                fixed = TRUE))), 
          data = data.frame(y, idx, n))
print(round(dig = 3, cbind(1/r$summary.fixed$sd^2, n)))

N <- 10
n <- (1:N)^2
y <- rnorm(N, sd = sqrt(1/n))
idx <- 1:N

r <- inla(y ~ 1, 
          scale = n, 
          data = data.frame(y, idx, n))
summary(r)

