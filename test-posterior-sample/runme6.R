n = 100
n.samples = 100
re = as.factor(sample(c("a",  "b"), n,  replace=TRUE))
y = -1 + as.numeric(re) + rnorm(n)

r = inla(y ~ 1 + f(rre, model="iid", hyper = list(prec = list(initial = -5, fixed=TRUE))),
         data = data.frame(y, rre=re),
         control.compute = list(config=TRUE))


samples = inla.posterior.sample(n.samples,  r)
f1 = inla.posterior.sample.eval(c("Intercept", "rre"), samples)

