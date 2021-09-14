n = 300
z = rnorm(n, sd=0.3)
eta = 1 + z
y.pred = inla.link.tan(eta)

kappa = 5
x = seq(-pi, pi, len = 10000)
d = exp(kappa*cos(x))
dd = cumsum(d)
dd = dd /max(dd)
cn.icdf.func = splinefun(dd, x, method = "monoH.FC")
rcn = function(n) cn.icdf.func(runif(n))
y = y.pred + rcn(n)

formula = y ~ 1 + z
r = inla(formula,  data = data.frame(y, z),
        family = "circularnormal",
        control.inla = list(cmin = -Inf))
