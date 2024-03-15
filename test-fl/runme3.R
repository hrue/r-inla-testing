inla.setOption(num.threads = "1:1")
n <- 10^6
x <- rnorm(n)
C <- matrix(runif(n*10), n, 10)
r <- inla(inla.mdata(C) ~ 1, family = "fl", data = list(C = C, x = x),
          control.inla = list(cmin = 99999999))
r$cpu.intern

y <- rpois(n, exp(2))
for (b in c("inla.mkl.work", INLA:::inla.call.builtin())) {
    rr <- inla(y ~ 1, family = "poisson", data = list(y = y),
               inla.call = b)
    print(rr$cpu.intern[grep("stage1", rr$cpu.intern)])

    rrr <- inla(y ~ 1, family = "binomial", data = list(y = y), Ntrials = max(y),
                inla.call = b)
    print(rrr$cpu.intern[grep("stage1", rrr$cpu.intern)])
}
