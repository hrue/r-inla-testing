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
formula = y ~ -1 + f(ii, model="iid", group = idx, control.group = list(model = "iid"))
fformula = y ~ -1 + f(ii, model="iid", replicate = idx)
ffformula = y ~ -1 + f(idx, model="iid", constr=FALSE)


r = inla(formula, data = data.frame(y, ii, idx),
        control.family = list(hyper = list(prec = list(initial = log(1/y.sd^2), fixed=TRUE))))
rr = inla(fformula, data = data.frame(y, ii, idx),
        control.family = list(hyper = list(prec = list(initial = log(1/y.sd^2), fixed=TRUE))))
rrr = inla(ffformula, data = data.frame(y, ii, idx),
        control.family = list(hyper = list(prec = list(initial = log(1/y.sd^2), fixed=TRUE))))

plot(r$summary.random$ii$mean)
lines(rr$summary.random$idx$mean)
lines(rrr$summary.random$idx$mean)
dif= r$summary.random$ii$mean - rr$summary.random$idx$mean
ddif= r$summary.random$ii$mean - rrr$summary.random$idx$mean
summary(dif)
summary(ddif)




