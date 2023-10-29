## simple example to illustrate the use of 'copy'
set.seed(1234)
N <- 100
n <- 30
u <- scale(rnorm(n))
s <- .1

i <- sample(1:n, N, replace = TRUE)
j <- sample(1:n, N, replace = TRUE)
k <- sample(1:n, N, replace = TRUE)
y <- u[i] + 2.5 * u[j] + 1.25 * u[k] + rnorm(N, sd = s)

r <- inla(y ~ -1 +
              f(i, values = 1:n) +
              f(j, copy = "i",
                hyper = list(
                    beta = list(fixed = FALSE))) +
              f(k, copy = "i",
                hyper = list(
                    beta = list(fixed = FALSE))), 
          data = data.frame(y, i, j, k),
          control.family = list(
              hyper = list(
                  prec = list(initial = log(1/s^2),
                              fixed = !TRUE))),
          verbose = TRUE)
