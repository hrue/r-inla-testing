## the precision parameter in the beta distribution
phi = 5


m = get("inla.models", envir = INLA:::inla.get.inlaEnv())
m$likelihood$beta$link = c(m$likelihood$beta$link, "log")
assign("inla.models", m, envir = INLA:::inla.get.inlaEnv())


## generate simulated data
n = 50
z = rnorm(n, sd=0.2)
eta = -2 + z
mu = exp(eta)/(1+exp(eta))
off = rep(-5, n)
a = mu * phi
b = -mu * phi + phi
y = rbeta(n, a, b)

## estimate the model
formula = y ~ 1 + z 
r = inla(formula, data = data.frame(y, z, off), family = "beta",
         control.compute = list(cpo=TRUE, dic=TRUE),
         control.family = list(control.link = list(model = "logit")), 
         verbose=TRUE)
##r$mode$x[1:n] = r$mode$x[1:n] -10
r = inla(formula, data = data.frame(y, z, off), family = "beta",
         control.compute = list(cpo=TRUE, dic=TRUE),
         control.mode = list(result=r, restart=TRUE), 
         control.family = list(control.link = list(model = "log")), 
         verbose=TRUE)

summary(r)
