library("gsl")
library("gamlss")

Pq = function(q)
{
    return (exp(0.25) / sqrt(8*pi) *
            (bessel_Knu((q+1)/2, 0.25) + bessel_Knu(abs((q-1)/2), 0.25)))
}
m1 = function(delta = 1, epsilon = 0)
{
    return (sinh(epsilon/delta)*Pq(1/delta))
}
m2 = function(delta = 1, epsilon = 0)
{
    return (0.5*(cosh(2*epsilon/delta)*Pq(2/delta) - 1))
}
m3 = function(delta = 1, epsilon = 0) 
{
    return (0.25*(sinh(3*epsilon/delta)*Pq(3/delta) -
                  3*sinh(epsilon/delta)*Pq(1/delta)))
}
m4 = function(delta = 1, epsilon = 0) 
{
    return ((1/8)*(cosh(4*epsilon/delta)*Pq(4/delta) -
                   4*cosh(2*epsilon/delta)*Pq(2/delta) + 3))
}

    

rsas = function(n,  mu = 0,  sigma = 1,  delta = 1, epsilon = 0)
{
    return (rSHASHo(n, mu, sigma, tau = delta, nu = epsilon))
}
dsas = function(x,  mu = 0,  sigma = 1,  delta = 1, epsilon = 0)
{
    return (dSHASHo(x, mu, sigma, tau = delta, nu = epsilon))
}
plot.dens = function(skew, kurt) {
    param = list(skew = skew, kurt = kurt)
    par = c(1, 0)
    fit = optim(par, fit.param, NULL, param = param)
    
    print(paste("par", fit$par))
    mm1 = m1(fit$par[1], fit$par[2])
    s = sqrt(m2(fit$par[1], fit$par[2]) - m1(fit$par[1], fit$par[2])^2)
    print(paste("m s", c(mm1, s)))
    
    par(mfrow=c(1, 2))
    xx = seq(mm1-6*s, mm1+6*s, len = 10000)
    plot(xx, log(dsas(xx, delta = fit$par[1],  epsilon = fit$par[2])), type="l")
    title(paste("logdens", "skew=", skew, ", kurt=", kurt))
    xx = seq(mm1-6*s, mm1+6*s, len = 10000)
    plot(xx, (dsas(xx, delta = fit$par[1],  epsilon = fit$par[2])), type="l")
    title(paste("dens", "skew=", skew, ", kurt=", kurt))

    return (list(fit.value = fit$value))
}


fit.param = function(x, param)
{
    delta = x[1]
    epsilon = x[2]

    ## these are standarized
    skew = param$skew
    kurt = param$kurt
    
    mm1 = m1(delta, epsilon)
    mm2 = m2(delta, epsilon)
    mm3 = m3(delta, epsilon)
    mm4 = m4(delta, epsilon)

    s = (mm3 -3*mm1*mm2 + 3*(mm1^2)*mm1 - mm1^3) /
        (mm2 - mm1^2)^(3/2)
    k = (mm4 -4*mm1*mm3 + 6*mm1^2 * mm2 - 4 * mm1^3 * mm1 + mm1^4) /
        (mm2 - mm1^2)^(4/2)

    return (((s-skew)^2 + (k - kurt)^2))
}


n = 10000
skew = -0.5
kurt = 3.4

param = list(skew = skew, kurt = kurt)
par = c(1, 0)
fit = optim(par, fit.param, NULL, param = param)
delta = fit$par[1]
epsilon = fit$par[2]
mu = m1(delta, epsilon)
s = sqrt(m2(delta, epsilon) - mu^2)

sas = rsas(n, mu = -mu/s, sigma = 1/s, delta = delta, epsilon =epsilon)

y.sd = 0.1
y = 0 + sas + rnorm(n, sd = 0.1)
yy = rep(0, n)

Y = matrix(NA, 2*n, 2)
Y[1:n, 1] = y
Y[1:n+n, 2] = yy

idx = c(1:n, 1:n)
formula = Y ~ -1 +
    f(idx, model="iid",
      hyper = list(prec = list(initial = -6, fixed=TRUE)))

r = inla(formula, data = list(Y=Y, idx=idx),
        family = list("gaussian", "sas"),
        control.family = list(
                list(hyper = list(prec = list(initial = log(1/y.sd^2), fixed=TRUE))), 
                list(hyper = list(kurt = list(initial = kurt, param = c(3 + 0.5*(kurt-3), 0.0001)),
                             skew = list(initial = 0.5*skew)))), 
        verbose = TRUE, keep=TRUE)

rr = inla(formula, data = list(Y=Y, idx=idx),
        family = list("gaussian", "gaussian"),
        control.family = list(
                list(hyper = list(prec = list(initial = log(1/y.sd^2), fixed=TRUE))), 
                list()),
        verbose = TRUE, keep=FALSE)


print(data.frame(r$mlik[1], rr$mlik[1]))

