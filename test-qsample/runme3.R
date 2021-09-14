inla.setOption(smtp = "pardiso")
N = 1000 ## number of samples
n = 1000
Q = matrix(rnorm(n^2), n, n)
Q = Q %*% t(Q)

for(opt in 1:6) {

    if (opt == 1) {
        M = 1
        T = 12
    } else if (opt == 2) {
        M = 2
        T = 6
    } else if (opt == 3) {
        M = 3
        T = 4
    } else if (opt == 4) {
        M = 4
        T = 3
    } else if (opt == 5) {
        M = 6
        T = 2
    } else if (opt == 6) {
        M = 12
        T = 1
    }


    inla.setOption(num.threads = T)
    inla.setOption(blas.num.threads = 1)

    ref = Sys.time()
    xx = mclapply(1:M, mc.cores = T, mc.preschedule = FALSE, 
                  function(...) {
        x = inla.qsample(N, Q)
        return (x)
    }, N = N)
    print(paste("M", M, "T", T, difftime(Sys.time(), ref,  unit = "sec"), "sec"))
}



