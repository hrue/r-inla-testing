n = 100
prec = 100

x = numeric(n)
x[] = 0
for(i in 3:n) {
    x[i] = 2*x[i-1] - x[i-1] + rnorm(1, sd = 1/sqrt(prec))
}

y.sd = 0.1
y = x + rnorm(n, sd = y.sd)
idx = 1:n
ii = rep(1, n)
formula = y ~ -1 + f(ii, model="iid", hyper = list(prec = list(initial = 0, fixed=TRUE)),
        group = idx,
        control.group = list(model = "rw2",
                hyper = list(prec = list(initial = log(prec), fixed=FALSE))))

fformula = y ~ -1 + f(idx, model="rw2", constr=FALSE, hyper = list(prec = list(initial = log(prec), fixed=FALSE)))


r = inla(formula, data = data.frame(y, ii, idx),
        control.data = list(hyper = list(prec = list(initial = log(1/y.sd^2), fixed=TRUE))),
        control.compute = list(q=TRUE, graph=TRUE),
        verbose = TRUE, keep=TRUE)

rr = inla(fformula, data = data.frame(y, ii, idx),
        control.data = list(hyper = list(prec = list(initial = log(1/y.sd^2), fixed=TRUE))), 
        control.compute = list(q=TRUE, graph=TRUE), 
        verbose = TRUE, keep=TRUE)

plot(r$summary.random$ii$mean)
lines(rr$summary.random$idx$mean)
dif= r$summary.random$ii$mean - rr$summary.random$idx$mean
summary(dif)




