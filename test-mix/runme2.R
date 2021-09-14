n = 10
s = 0.5
E = runif(n, min=0.5, max=2.0)
y = rpois(n, lambda = E*exp(1 + rnorm(n, sd=s)))
param = c(1, 0.1)
npoints = 71
hyper = list(prec = list(
                 initial = 0,
                 prior = "loggamma", 
                 param = param, 
                 fixed = FALSE))

integrate_pois = function(x, y, E, prec)
{
    s = sqrt(1/prec)
    dx = s/10
    xx = s*seq(-6, 6, by = dx)
    return (log(sum(dpois(y, E*exp(x + xx)) * dnorm(xx, mean=0, sd = s) * diff(xx)[1])))
}

r = inla(
    formula = y ~ 1, 
    data = data.frame(y, E),
    family = "poisson",
    E = E, 
    control.family = list(
        control.mix = list(
            model = "gaussian",
            ##integrator = "simpson", 
            integrator = "quadrature", 
            npoints = npoints, 
            hyper = hyper)),
    control.fixed = list(prec.intercept = 1), 
    verbose = TRUE,
    keep=TRUE)

r.cpu = r$cpu
r = inla.rerun(r)
r = inla.rerun(r)
r = inla.hyperpar(r)

rr = inla(
    formula = y ~ 1 + f(idx, model="iid", hyper = hyper), 
    data = data.frame(y, E, idx=1:n), 
    family = "poisson",
    E = E,
    control.fixed = list(prec.intercept = 1), 
    verbose = TRUE)
rr.cpu = rr$cpu
rr = inla.rerun(rr)
rr = inla.rerun(rr)
rr = inla.hyperpar(rr)

plot(inla.smarginal(r$internal.marginals.hyperpar[[1]]),
     main = "posterior for log(prec)",
     type="l", lwd=5, col = "red")
lines(inla.smarginal(rr$internal.marginals.hyperpar[[1]]),
      lwd=3, col="yellow")

print(c(r$mode$theta, rr$mode$theta))
print(r$mlik - rr$mlik)

print(r.cpu)
print(rr.cpu)
