n = 100
Q = matrix(rnorm(n^2), n, n)
Q = Q %*% t(Q)

selection = sample(0:1, size=n,  replace=TRUE)
x = inla.qsample(1, Q=Q,  selection = selection)
