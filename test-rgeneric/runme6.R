n = 100
r = rep(1:10, each=10)
g = rep(10:1, each=10)
y = rnorm(n)
idx = 1:n
model = inla.rgeneric.define(inla.rgeneric.iid.model, n=n)
formula = y ~ -1 + f(idx, model="rgeneric", n=n, rgeneric = model, replicate = r, group=g,
                     control.group = list(model="iid"))
                                          
r1 = inla(formula, data = data.frame(y, idx, r, g),
         family = "gaussian", keep=T, 
         control.family = list(hyper = list(prec = list(initial = 5,  fixed=TRUE))))
