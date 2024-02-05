## this is how to 'fake it' with a MVN observations. the indexing might be a bit ackward; see
## inla.doc("iidkd")

library(INLA)
library(mvtnorm)

n <- 1000
m <- 5
N <- n*m

A <- matrix(rnorm(m^2), m, m)
Sigma <- A %*% t(A)
##iSigma <- 1/sqrt(diag(Sigma))
##Sigma <- diag(iSigma) %*% Sigma %*% diag(iSigma)
Sigma <- 0.5 * (Sigma + t(Sigma)) ## force it to be symmetric

y <- c()
x <- c()
Eps <- c()
for(i in 1:n) {
    eps <- as.numeric(rmvnorm(1, sigma = Sigma))
    xx <- rnorm(m)
    eeta <- 1:m + xx
    yy <- eeta + eps
    
    y <- c(y, yy)
    x <- c(x, xx)
    Eps <- cbind(Eps, eps)
}

X <- matrix(0, N, m)
for(k in 1:m) {
    X[seq(k, N, by = m), k] <- x[seq(k, N, by = m)]
}

idx <- c()
for(i in 1:n)
    for(j in 1:m)
        idx <- c(idx, (j-1)*n + i)


w <- rep(-1, N)
intercept <- as.factor(rep(1:m, n))
r <- inla(y ~ -1 + intercept + X +
              f(idx, w, model = "iidkd", order = m, n = N,
                hyper = list(theta1 = list(
                                 param = c(100, rep(1, m), rep(0, m*(m-1)/2))))), 
          family = "gaussian",
          control.family = list(hyper = list(prec = list(initial = 15, fixed = TRUE))),
          data = list(y = y, intercept = intercept, X = X, idx = idx, m = m, N = N, w = w),
          control.inla = list(int.strategy = "eb"), ## just for testing
          verbose = TRUE)

## this is how the internal parameters are defined
L <- t(chol(solve(Sigma)))
diag(L) <- log(diag(L))
LL <- t(chol(solve(cov(t(Eps)))))
diag(LL) <- log(diag(LL))
## compare the estimated (internal) parameters with the truth
round(dig = 3, cbind(true = c(diag(L), L[lower.tri(L)]),
                     mle = c(diag(LL), LL[lower.tri(LL)]),
                     inla = r$mode$theta))

## see inla.doc("iidkd")
xx <- inla.iidkd.sample(10^4, r, "idx")
qq <- matrix(rowMeans(matrix(unlist(xx), nrow = m^2)), m, m)
iSigma <- 1/sqrt(diag(Sigma))
Cor <- diag(iSigma) %*% Sigma %*% diag(iSigma)
round(dig = 3, cbind(inla = c(diag(qq), qq[lower.tri(qq)]),
                     true = c(sqrt(diag(Sigma)), Cor[lower.tri(Cor)])))
