n = 1000
x = 0.995*scale(rnorm(n))
eta = 3 + x
y = rpois(n, lambda = exp(eta))
yc = y+0.5

r = inla(yc ~ 1 + x, data = data.frame(yc, x), family = "contpoisson", verbose=F)
rr = inla(y ~ 1 + x, data = data.frame(y, x), family = "poisson", verbose=F)

r$mlik
rr$mlik

