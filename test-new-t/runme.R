check.it = function() {
    x = read.table("l.dat")
    x = x[, c(2, 4)]
    sum(exp(x[, 2]))*(x[2, 1] - x[1, 1])
}

df = 20
n = 100L
nstrata = 3L
ntot = n * nstrata

z = rnorm(ntot)
y = numeric(ntot)
k = 0L
for(i in 1L:nstrata) {
    j = 1L:n
    y[k + j] = 1 + z[k+j] + rt(n, df=df) * i
    k = k + n
}

strata = rep(1L:nstrata, each = n)
i = 1L:ntot
formula = y ~ 1 + z

h = 0.01
r = inla(formula,
        data = data.frame(y, z, strata),
        family = "tstrata",
        verbose=TRUE,
        strata = strata,
        control.data = list(
                hyper = list(dof = list(
                                     initial = log(df-5),
                                     fixed=FALSE,
                                     param=c(1, .1)))), 
        control.inla = list(
                verbose=FALSE,
                h=h))


stop("XXXXXXXXXXXXX")




zz = c()
sdzz = c()
r = NULL
for (df in 6:25) {
    r = inla(formula,  data = data.frame(y, z, strata), family = "tstrata", verbose=FALSE,
            strata = strata,
            control.data = list(hyper = list(dof = list(initial = log(df-5),
                                                     fixed=FALSE,
                                                     prior = "loggamma", 
                                                     param = c(1, 0.1))), 
                    variant = 0),
            control.mode = list(result = r, restart=TRUE), 
            control.inla = list(
                    verbose=FALSE,
                    h = h))
    zz = c(zz, r$summary.fixed["z", "mean"])
    sdzz = c(sdzz, r$summary.fixed["z", "sd"])
    print(df)
}
