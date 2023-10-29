## simple example to illustrate the use of 'copy'
set.seed(1234)
N <- 1000
n <- 300
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
                              fixed = TRUE))),
          verbose = TRUE)
plot(u, r$summary.random$i$mean, pch = 19)
abline(a = 0, b = 1, lwd = 3, col = "blue")
stop("XXXXXXX")
## estimate scaling parameters, assuming
## y <- u[i] + beta.j * u[j] + beta.k * u[k] + rnorm(N, sd = s)
## where the true values are beta.j=1 and beta.k=1

for (e in 7:10) {
    rr <- inla(y ~ -1 +
                   f(i, values = 1:n) +
                   f(j, copy = "i",
                     precision = 10^e,
                     hyper = list(
                         beta = list(fixed = FALSE))) +
                   f(k, copy = "i",
                     precision = 10^e,
                     hyper = list(
                         beta = list(fixed = FALSE))), 
               ##num.threads = "1:1",
               control.inla = list(tolerance = 0.005), 
               data = data.frame(y, i, j, k),
               control.family = list(hyper = list(
                                         prec = list(initial = log(1/s^2),
                                                     fixed = TRUE))))
    print(c(e, rr$misc$nfunc, rr$summary.hyperpar[2:3,1]))
}
rr$summary.hyperpar[,c("mean","sd")]
inla.dev.new()
plot(u, rr$summary.random$i$mean, pch = 19)
abline(a = 0, b = 1, lwd = 3, col = "blue")

