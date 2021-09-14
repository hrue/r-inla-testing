## model:
##     y ~ \mu + eps
##     eps ~ N(0,  var.eps)
##     \mu ~ N(\gamma, var.mu)
## which gives,  marginally, 
##     y ~ N(\gamma,  var.eps + var.mu)

y=rnorm(1)
var.mu = rexp(1)
var.eps = rexp(1)
mu.mu = rnorm(1)

e.y = mu.mu
var.y = var.mu + var.eps

print("test intercept")
r = inla(y ~ 1,
    family  = "gaussian",
    control.family = list(
        hyper = list(prec = list(initial = log(1/var.eps), fixed=TRUE))), 
    data = data.frame(y),
    control.fixed = list(
        prec.intercept = 1/var.mu,
        mean.intercept = mu.mu))
r$mlik
dnorm(y, mean = e.y, sd = sqrt(var.y), log=TRUE)

print("test fixed")
r = inla(y ~ -1 + x,
    family  = "gaussian",
    control.family = list(
        hyper = list(prec = list(initial = log(1/var.eps), fixed=TRUE))), 
    data = data.frame(y, x=1),
    control.fixed = list(
        prec = 1/var.mu,
        mean = mu.mu))
r$mlik
dnorm(y, mean = e.y, sd = sqrt(var.y), log=TRUE)

print("test offset")
r = inla(y ~ -1 + x + offset(off),
    family  = "gaussian",
    control.family = list(
        hyper = list(prec = list(initial = log(1/var.eps), fixed=TRUE))), 
    data = data.frame(y, off = e.y, x=1),
    control.fixed = list(
        prec = 1/var.mu,
        mean = 0))
r$mlik
dnorm(y, mean = e.y, sd = sqrt(var.y), log=TRUE)

