nf <- 3
n <- 300 * nf

x <- rnorm(n)
xx <- rnorm(n)
Y <- matrix(NA, n, nf)
for(i in 1:n) {
    idx <- sample(1:nf, 1)
    Y[i, idx] <- 1 + x[i] + xx[i] + x[i] * xx[i] + rnorm(1, sd = 0.1)
}

for (mode in c("compact", "classic")) {
    cat("\n\n\n")
    print(mode)

    r <- inla(Y ~ x*xx,
              data = list(Y = Y, x = x, xx = xx),
              family = rep("t", nf),
              inla.mode = mode, 
              inla.call = INLA:::inla.call.builtin())
    r <- inla.rerun(r)

    rr <- inla(Y ~ x*xx,
               data = list(Y = Y, x = x, xx = xx),
               family = rep("t", nf),
               inla.mode = mode, 
               inla.call = "inla.mkl.work")
    rr <- inla.rerun(rr)

    print(r$mlik - rr$mlik)
    print(r$summary.fixed[,  c("mean", "sd")]- rr$summary.fixed[,  c("mean", "sd")])
    print(r$internal.summary.hyperpar[, c("mean", "sd")]- rr$internal.summary.hyperpar[, c("mean", "sd")])
    print(r$mode$theta - rr$mode$theta)
}

