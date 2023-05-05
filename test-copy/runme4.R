n <- 3
idx <- 1:n
pr <- c(10, 100)
r <- inla(y ~ -1 +
              f(idx) +
              f(idxx, copy = "idx", precision = pr[1]), 
          data = data.frame(y = rep(NA, n), idx, idxx = idx, idxxx = idx),
          control.mode = list(theta = c(0, 0), fixed = TRUE),
          control.compute = list(config = TRUE))
r$misc$configs$config[[1]]$Q

Qfun <- function(x) {
    return (0.5 * (sum(x[1:n]^2) + pr[1] * sum((x[n + 1:n] - x[1:n])^2)))
}
library(numDeriv)
H <- hessian(Qfun, rep(0, 2*n))
H[lower.tri(H)] <- 0
round(dig = 2, inla.as.sparse(H))


rr <- inla(y ~ -1 +
              f(idx) +
              f(idxx, copy = "idx", precision = pr[1]) + 
              f(idxxx, copy = "idxx", precision = pr[2]), 
          data = data.frame(y = rep(NA, n), idx, idxx = idx, idxxx = idx),
          control.mode = list(theta = c(0, 0), fixed = TRUE),
          control.compute = list(config = TRUE))
rr$misc$configs$config[[1]]$Q

QQfun <- function(x) {
    return (0.5 * (sum(x[1:n]^2) + pr[1] * sum((x[n + 1:n] - x[1:n])^2) + pr[2] * sum((x[2*n + 1:n] - x[n+1:n])^2)))
}
H <- hessian(QQfun, rep(0, 3*n))
H[lower.tri(H)] <- 0
round(dig = 2, inla.as.sparse(H))


rrr <- inla(y ~ -1 +
              f(idx) +
              f(idxx, copy = "idx", precision = pr[1]) + 
              f(idxxx, copy = "idx", precision = pr[2]), 
          data = data.frame(y = rep(NA, n), idx, idxx = idx, idxxx = idx),
          control.mode = list(theta = c(0, 0), fixed = TRUE),
          control.compute = list(config = TRUE))
rrr$misc$configs$config[[1]]$Q

QQQfun <- function(x) {
    return (0.5 * (sum(x[1:n]^2) + pr[1] * sum((x[n + 1:n] - x[1:n])^2) + pr[2] * sum((x[2*n + 1:n] - x[1:n])^2)))
}
H <- hessian(QQQfun, rep(0, 3*n))
H[lower.tri(H)] <- 0
round(dig = 2, inla.as.sparse(H))
