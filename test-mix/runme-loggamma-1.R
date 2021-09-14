n = 1000
x = rnorm(n, sd = 1/3)
npoints = 51
beta = 1.0
prec = 3
intercept = 1
s = 0.2
re = log(rgamma(n, shape = prec, rate = prec))
eps = rnorm(n, sd = s)
eta = intercept + beta * x + re
y = eta + eps
r = inla(
    formula = y ~ 1 + x, 
    data = data.frame(y, x), 
    family = "gaussian", 
    control.fixed = list(prec.intercept = 1, prec = 1), 
    control.family = list(
        list(
            hyper = list(prec = list(
                             initial = log(1/s^2), 
                             fixed = TRUE)), 
            control.mix = list(
                model = "loggamma",
                npoints= npoints,
                hyper = list(prec = list(
                                 initial = log(prec), 
                                 prior = "pc.mgamma", 
                                 param = 10))))), 
    verbose =TRUE)
summary(r)
print(list(theta.mode = as.numeric(r$mode$theta), theta.true = log(prec)))

plot(r$internal.marginals.hyperpar[[1]],  type = "l")
abline(v = log(prec), lwd=5)

eta = intercept + beta * x - re 
y = eta + eps
rr = inla(
    formula = y ~ 1 + x, 
    data = data.frame(y, x), 
    family = "gaussian", 
    control.fixed = list(prec.intercept = 1, prec = 1), 
    control.family = list(
        list(
            hyper = list(prec = list(
                             initial = log(1/s^2), 
                             fixed = TRUE)), 
            control.mix = list(
                model = "mloggamma",
                npoints = npoints,
                hyper = list(prec = list(
                                 initial = log(prec), 
                                 param = 7))))), 
    verbose =TRUE)
summary(rr)
print(list(theta.mode = as.numeric(rr$mode$theta), theta.true = log(prec)))

## should be close, only asymptotically the same
plot(r$internal.marginals.hyperpar[[1]],  type = "l", main="log(prec). Should be close, not equal.")
lines(rr$internal.marginals.hyperpar[[1]],  type = "l")
abline(v = log(prec), lwd=5)
