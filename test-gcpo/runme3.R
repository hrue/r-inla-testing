library(INLA)
inla.setOption(inla.mode = "experimental")

mk.Q <- function(n) {
    A <- matrix(rnorm(n^2), n, n)
    Q <- A %*% t(A)
    S <- solve(Q)
    S <- S / mean(diag(S))
    Q <- solve(S)
    return (Q)
}

set.seed(123)
n1 <- 5
n <- n1 + n1
Q1 <- mk.Q(n1)
Q2 <- mk.Q(n1)
S1 <- solve(Q1)
S2 <- solve(Q2)

if (!exists("y")) {
    s <- 1
    y <- c(inla.qsample(1, Q1)) + c(inla.qsample(1, Q2))+ rnorm(n1, sd = s)
}

A <- diag(n1)
S <- A %*% (S1 + S2) %*% t(A)
Q <- solve(S)
diag(Q) <- diag(Q) + 1/s^2
SS <- solve(Q)
mmean <- SS %*% (1/s^2 * y)

SS.orig <- SS

Sys.setenv(INLA_gcpo = TRUE)
r <- inla(
    y ~ -1 +
        f(idx1, model = "generic", Cmatrix = Q1, hyper = list(prec = list(initial = 0, fixed = TRUE))) +
        f(idx2, model = "generic", Cmatrix = Q2, hyper = list(prec = list(initial = 0, fixed = TRUE))),
    data = data.frame(y, idx1 = 1:n1, idx2 = 1:n1),
    family = "gaussian",
    keep = T, 
    control.family = list(hyper =  list(prec = list(initial = log(1/s^2), fixed = TRUE))),
    inla.call = "inla.mkl.work",
    num.threads = "1:1",
    verbose = TRUE)


w <- rep(1, n1)
idx <- c(1, 2, 3)
idx <- c(3, 4, 5)
w[idx] <- 0

print(SS[idx, idx])

D <- diag(diag(w))
QQ <- solve(S)
diag(QQ) <- diag(QQ) + 1/s^2 * D
SS <- solve(QQ)
mm <- SS %*% (1/s^2 *  diag(D)  %*% y)


print(c(mm[1], sqrt(SS[1, 1])))
