n = 1000
A = matrix(rnorm(n^2) * rbinom(n^2, prob = 20/n, size = 1), n, n)
A = A %*% t(A)
diag(A) <- diag(A) + sqrt(n)

inla.setOption(smtp="taucs", inla.call = "inla.mkl.work", num.threads = "4:1")
Sys.setenv(INLA_TRACE = "GMRFLib_solve_llt_sparse_matrix")

for(nb in 1:320) {
    b <- matrix(rnorm(n*nb), n, nb)
    tref <- Sys.time()
    xx = inla.qsolve(A, b)
    print(c(nb, Sys.time() - tref))
}

