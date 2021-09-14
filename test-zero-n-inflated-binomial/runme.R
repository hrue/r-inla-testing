source("ZeroNinflated.r")

n = 5000
alpha1 = 0.2
alpha2 = 0.5
z = rnorm(n)
x = 0 + z
N = rep(6, n)

y = ZeroNinflated(N, alpha1, alpha2, x)[[1]]

formula = y ~ 1 + z
r = inla(formula,  data = data.frame(y, z, N),
        family = "zeroninflatedbinomial2",
        verbose=TRUE, Ntrials = N)

if (FALSE) {
    rr = inla(formula,  data = data.frame(y, z, N),
            family = "zeroninflatedbinomial2",
            verbose=TRUE, Ntrials = N,
            inla.call = "inla.work")
}



