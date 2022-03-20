## A simple model
n = 8
a = rnorm(n)
b = rnorm(n)
stopifnot(n/2 == n %/% 2)
idx = rep(1:(n%/%2), each=2)
idxx = c(1,2,rep(3,n-2))

x.idx = rnorm(n)[idx]
x.idxx = rnorm(n)[idxx]

y = a + b + x.idx + x.idxx
formula = y ~ 1 + a + b + f(idx, model="iid",constr=T) + f(idxx,model="iid",constr=T)

lc1 = inla.make.lincomb(a=2, b=3,
        idx = c(10,-1,rep(NA,n-2)),
        idxx = c(NA,NA,3))
names(lc1) = "lc1"

lc2 = inla.make.lincomb(idx = c(1, 2), "(Intercept)" = 1)
names(lc2) = "lc2"
all.lc = c(lc1,lc2)

inla.setOption(inla.call = "inla.mkl")
r = inla(formula, data = data.frame(a,b,y,idx,idxx),
        lincomb = all.lc,
        control.compute = list(return.marginals=TRUE),
        control.predictor = list(compute=TRUE),
        control.family = list(initial=10, fixed=TRUE))

inla.setOption(inla.call = "inla.mkl.work")
r.work = inla(formula, data = data.frame(a,b,y,idx,idxx),
        lincomb = all.lc, verbose = T, 
        control.predictor = list(compute=TRUE),
        control.family = list(initial=10, fixed=TRUE))

print(r$summary.lincomb.derived[, "mean"])
print(r.work$summary.lincomb.derived[, "mean"])
print(r$summary.lincomb.derived[, "sd"])
print(r.work$summary.lincomb.derived[, "sd"])
