inla.setOption("inla.call", "inla.work")
inla.my.update()

n = 100
ooffset = 1:100
y = ooffset + rnorm(n)
formula = y ~ offset(ooffset)-1
r = inla(formula, data = data.frame(y,ooffset), keep=TRUE, verbose=TRUE,
        control.predictor = list(compute=TRUE))

rr = inla(formula, data = data.frame(y,ooffset), keep=TRUE, verbose=F,
        control.predictor = list(compute=TRUE),
        control.mode = list(result = r, restart = TRUE))
