n = 20
nc = 5
nlc = 4

if (!exists("y")) {
    y = rnorm(n)
    idx = 1:n
    constr = list(A = matrix(runif(nc*n), nc, n), e = rnorm(nc))
    lincombs = inla.make.lincombs(idx = matrix(runif(nlc*n), nlc, n))
}
formula = y ~ 1 + f(idx, extraconstr = constr, initial = 0,  fixed=TRUE)

r = inla(formula, data = data.frame(y, idx), 
         lincomb = lincombs, 
         control.family = list(hyper = list(prec = list(initial = 1, fixed=TRUE))),
         control.inla = list(lincomb.derived.correlation.matrix=TRUE, int.strategy = "eb"), 
         control.predictor = list(initial = 8), 
         control.fixed = list(prec.intercept = 1), 
         control.compute = list(smtp = "taucs", openmp.strategy = "small"), 
         verbose = TRUE)

print(r$misc$lincomb.derived.correlation.matrix)

INLA:::inla.my.update(b=T)
inla.setOption(pardiso.license = "/home/hrue/sys/licenses/pardiso.lic")
r.pardiso.s = inla(formula, data = data.frame(y, idx), 
                   control.compute = list(openmp.strategy = "pardiso.serial"), 
                   lincomb = lincombs, 
                   control.family = list(hyper = list(prec = list(initial = 1, fixed=TRUE))),
                   control.inla = list(lincomb.derived.correlation.matrix=TRUE, int.strategy = "eb"), 
                   control.fixed = list(prec.intercept = 1), 
                   control.predictor = list(initial = 8), 
                   verbose = TRUE, keep=TRUE)

print(r.pardiso.s$misc$lincomb.derived.correlation.matrix)

r.pardiso.p = inla(formula, data = data.frame(y, idx), 
                   control.compute = list(openmp.strategy = "pardiso.parallel"), 
                   lincomb = lincombs, 
                   control.family = list(hyper = list(prec = list(initial = 1, fixed=TRUE))),
                   control.inla = list(lincomb.derived.correlation.matrix=TRUE, int.strategy = "eb"), 
                   control.fixed = list(prec.intercept = 1), 
                   control.predictor = list(initial = 8), 
                   verbose = TRUE)

print(r$misc$lincomb.derived.correlation.matrix)
print(r.pardiso.s$misc$lincomb.derived.correlation.matrix)
print(r.pardiso.p$misc$lincomb.derived.correlation.matrix)

