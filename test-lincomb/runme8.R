n = 5
Cov = matrix(runif(n^2), n, n)
Cov = Cov %*% t(Cov)
stopifnot(all(eigen(Cov)$values > 0))
Q = solve(Cov)

Corr = matrix(NA, n, n)
for(i in 1:n) {
    for(j in 1:n) {
        Corr[i, j] = Cov[i, j] / sqrt(Cov[i, i]*Cov[j, j])
    }
}

A = matrix(runif(2*n), 2, n)
Cov.correction = Cov %*% t(A) %*% solve( A %*% Cov %*% t(A) ) %*% A %*% Cov
Cov.A = Cov - Cov.correction
Corr.A = matrix(NA, n, n)
for(i in 1:n) {
    for(j in 1:n) {
        Corr.A[i, j] = Cov.A[i, j] / sqrt(Cov.A[i, i]*Cov.A[j, j])
    }
}
e = rep(0, 2)

idx = 1:n
idx2 = 1:n
formula = y ~ -1 + f(idx, model="generic", Cmatrix = Q, hyper = list(prec = list(initial = 0, fixed=TRUE)), 
        extraconstr = list(A=A, e=e)) + f(idx2, param = c(1000, 0.0001))

B = matrix(runif(n^2), n, n)
lc = inla.make.lincombs("Predictor" = B)

Cov.B =  B %*% Cov.A %*% t(B)
Corr.B = matrix(NA, n, n)
for(i in 1:n) {
    for(j in 1:n) {
        Corr.B[i, j] = Cov.B[i, j] / sqrt(Cov.B[i, i]*Cov.B[j, j])
    }
}

y = rep(0, n)
r = inla(formula,
          data = data.frame(y, idx, idx2),
          lincomb = lc, verbose=TRUE, keep=TRUE,
          control.family = list(hyper = list(prec = list(initial = -12, fixed=TRUE))), 
          control.results = list(return.marginals.random=FALSE), 
          control.inla = list(lincomb.derived.correlation.matrix = TRUE, 
              int.strategy = "grid",  diff.logdens = 10))

mean(abs(r$misc$lincomb.derived.correlation.matrix - Corr.B))
mean(abs(r$misc$lincomb.derived.covariance.matrix - Cov.B))
