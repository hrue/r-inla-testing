n = 1000
x = rnorm(n)
eps = rnorm(n)


yy = 10*(1 + x) + eps
y = 10*(1 + x) + pmin(1.0, pmax(-1, eps))

r = inla(y ~ 1 + x,
        family = "gaussianwindow",
        data = data.frame(y, x),
        control.family = list(fixed=FALSE), 
        control.predictor = list(compute=TRUE), 
        verbose=TRUE)

