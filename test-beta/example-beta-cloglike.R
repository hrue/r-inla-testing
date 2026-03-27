n = 10^4
z = rnorm(n, sd=0.3)
zz = rnorm(n, sd=0.4)
eta = 1 + z
phi <- exp(1 + zz)

mu = exp(eta)/(1+exp(eta))
a = mu * phi
b = -mu * phi + phi
y = rbeta(n, a, b)

Y <- inla.mdata(cbind(y, zz))
cloglike <- inla.cloglike.define(model = "inla_cloglike_beta", 
                                 shlib = "cloglike-demo-beta.so")
formula = Y ~ 1 + z
r = inla(formula,
         data = list(Y = Y), 
         family = "cloglike",
         control.family = list(cloglike = cloglike))
summary(r)
