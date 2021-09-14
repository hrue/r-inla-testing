set.seed(123)

## A simple model
n = 20
a = rnorm(n)
b = rnorm(n)
stopifnot(n/2 == n %/% 2)
idx = rep(1:(n%/%2), each=2)
idxx = c(1,2,rep(3,n-2))

x.idx = rnorm(n)[idx]
x.idxx = rnorm(n)[idxx]

y = a + b + x.idx + x.idxx
y[1]=NA
formula = y ~ 1 + a + b + f(idx, model="iid",constr=T) + f(idxx, n=n, values=1:n,model="iid",constr=T)

lc1 = inla.make.lincomb(a=2, b=3,
        idx = c(10,-1,rep(NA,n-2)),
        idxx = c(NA,NA,3))
names(lc1) = "lc1"

lc2 = inla.make.lincomb(Predictor = c(NA, 1, -2, rep(NA,n-2)), "(Intercept)" = 1)
names(lc2) = "lc2"

lc3 = inla.make.lincomb(idxx = c(rep(NA,n-1),1))
names(lc3) = "lc3"

all.lc = c(lc1,lc2,lc3)


inla.setOption("inla.call", "inla.work")
inla.my.update()

r.work = inla(formula, data = data.frame(a,b,y,idx,idxx),
        lincomb = all.lc,
        control.inla = list(lincomb.derived.only=FALSE),
        control.lincomb = list(precision = 10^8,verbose=F),
        control.predictor = list(compute=TRUE),
        control.family = list(initial=10, fixed=TRUE),
        verbose=FALSE,
        keep=F)

