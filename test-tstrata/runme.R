df = 5
n = 10000L
nstrata = 5L
ntot = n * nstrata

z = rnorm(ntot)
y = numeric(ntot)
k = 0L
for(i in 1L:nstrata) {
    j = 1L:n
    y[k + j] = 1 + z[k+j] + rt(n, df=df) / sqrt(df/(df-2)) 
    k = k + n
}

strata = rep(1L:nstrata, each = n)
i = 1L:ntot
formula = y ~ 1 + z

hyper.t = list(dof = list(param = c(df, 0.5)),
               prec1 = list(initial = 0,
                           fixed = !TRUE))
r = inla(formula,
         data = data.frame(y, z, strata),
         family = "tstrata",
         verbose=TRUE,
         strata = strata,
         control.inla = list(verbose=F, cmin = Inf),
         control.family = list(hyper = hyper.t))

hyper.t = list(dof = list(param = c(df, 0.5)),
               prec = list(initial = 0,
                           fixed = TRUE))
rr = inla(formula,
         data = data.frame(y, z, strata),
         family = "t",
         verbose=TRUE,
         control.inla = list(verbose=F, cmin = Inf),
         control.family = list(hyper = hyper.t))

