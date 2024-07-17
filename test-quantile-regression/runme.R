rkumar = function(eta, phi, q=0.5)
{
    n = length(eta)
    kappa = eta
    beta = log(1-q)/log(1-exp(-phi))
    alpha = log(1- (1-q)^(1/beta)) / log(kappa)
    u = runif(n)
    y = (1-u^(1/beta))^(1/alpha)
    return (y)
}

inla.setOption(num.threads = "4:1")

n = 10^5
q = 0.9
phi = 2
x = rnorm(n, sd = 1)
eta = inla.link.invlogit(1 + x)
y = rkumar(eta, phi, q)

r = inla(y ~ 1 + x,
    data = data.frame(y, x),
    family = "qkumar",
    control.compute = list(cpo=TRUE), 
    control.family = list(control.link = list(quantile = q)),
    verbose=!TRUE)

rr = inla(y ~ 1 + x,
    data = data.frame(y, x),
    family = "qkumar",
    control.compute = list(cpo=TRUE), 
    control.family = list(control.link = list(quantile = q)),
    verbose=!TRUE, inla.call = "inla.mkl.work")
r$mlik - rr$mlik
r$cpu.intern
rr$cpu.intern



