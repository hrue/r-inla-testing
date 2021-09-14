set.seed(123)
n = 1000
z = rnorm(n, sd=0.3)
z = z - mean(z)
eta = 1 + z
y.pred = inla.link.tan(eta)

assign("enable.model.likelihood.wrappedcauchy", TRUE,  INLA:::inla.get.inlaEnv())

rho = 0.8
x = seq(-pi, pi, len = 10000)
d = 1/(1+rho^2 - 2*rho*cos(x))
dd = cumsum(d)
dd = dd /max(dd)
rwc.icdf.func = splinefun(dd, x)
rwc.icdf = function(n) rwc.icdf.func(runif(n))
y = y.pred + rwc.icdf(n)

formula = y ~ 1 + z
r=inla(formula,  data = data.frame(y, z), family = "wrappedcauchy",
        control.fixed = list(correlation.matrix=TRUE),
        control.inla = list(verbose=FALSE, cmin = 10,
                int.strategy="grid", diff.logdens=10, dz=0.2))

