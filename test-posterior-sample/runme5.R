n = 100
n.samples = 100
re = as.factor(sample(c("a",  "b"), n,  replace=TRUE))
y = -1 + as.numeric(re) + rnorm(n)

r = inla(y ~ -1 + f(rre, model="iid", hyper = list(prec = list(initial = -5, fixed=TRUE))),
         data = data.frame(y, rre=re),
         control.compute = list(config=TRUE))


fun = function(zz = NA, new.Event = NA) {
    print(str(rre))
}
samples = inla.posterior.sample(n.samples,  r)
f1 = inla.posterior.sample.eval(fun, samples, zz = znew, new.Event = c(1,0))

