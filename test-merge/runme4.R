n = 10
x = rnorm(n)
y = 1 + x + rnorm(n, sd = 0.2)

r = inla(y ~ 1 + x,
         data = data.frame(y, x),
         control.inla = list(int.strategy = "grid",
                             dz = 0.25,  diff.logdens = 8), 
         control.compute = list(cpo = TRUE, po = TRUE))

a = r$joint.hyper
a[, 2] = exp(a[, 2] - max(a[, 2]))

rr = rep(list(list()), nrow(a))
for(k in 1:nrow(a)) {
    rr[[k]] = inla(y ~ 1 + x,
                   data = data.frame(y, x),
                   control.compute = list(cpo = TRUE, po = TRUE),
                   control.family = list(hyper = list(prec = list(initial = a[k, 1], fixed=TRUE))))
}

prob = a[, 2]
r.merge = inla.merge(rr, prob)

