df = 15
n = 100L
nstrata = 5L
ntot = n * nstrata

z = rnorm(ntot)
y = numeric(ntot)
k = 0L
for(i in 1L:nstrata) {
    j = 1L:n
    y[k + j] = 1 + z[k+j] + rt(n, df=df) / sqrt(df/(df-2)) * i
    k = k + n
}

strata = rep(1L:nstrata, each = n)
i = 1L:ntot
formula = y ~ 1 + z

h = 0.01
hyper.t = list(dof = list(fixed=TRUE))
r = inla(formula,  data = data.frame(y, z, strata), family = "tstrata", verbose=TRUE, strata = strata,
        control.inla = list(h=h, verbose=F),  control.data =list(hyper = hyper.t),
        control.mode = list(theta = rep(3, nstrata), restart=TRUE),
        keep=TRUE)

hyper.t$dof$fixed = FALSE
hyper.t$dof$param = c(1, 0.001)

r = inla(formula,  data = data.frame(y, z, strata), family = "tstrata", verbose=TRUE, strata = strata,
        control.inla = list(h=h, verbose=F),
        control.data = list(hyper = hyper.t),
        control.mode = list(theta = c(3, r$mode$theta),  x=r$mode$x, restart=TRUE))
