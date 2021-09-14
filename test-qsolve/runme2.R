n = 1000
A = matrix(rnorm(n^2), n, n)
A = A %*% t(A)

inla.setOption(smtp="pardiso")
##xx = inla.qsolve(A, A)

xx = inla.qsample(n, A)


