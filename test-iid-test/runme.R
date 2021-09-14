inla.my.update(b=T)
inla.setOption(num.threads=8)

n = 1
ng = 3
nr = 1
N = n*ng*nr

idx = rep(1:n, nr*ng)
g = rep(1:ng, each = n*nr)
r = rep(rep(1:nr, each = n), ng)

y = rep(0, N)

formula = y ~ 0 + f(idx, model="iidtest", replicate=r, group=g,
        initial = 0, fixed=TRUE, param=c(1, 1), diagonal = 0,
        control.group = list(model = "ar1", initial = 0,  fixed = TRUE))
res = inla(formula, data = data.frame(y, idx, r, g), verbose=TRUE,
        control.family = list(hyper = list(prec = list(initial = 0, fixed=TRUE))),
        control.predictor = list(compute=TRUE),
        control.inla = list(int.strategy = "eb",  diff.logdens = 10, dz=.01),
        keep=TRUE)

res$summary.random$idx$mean
res$summary.random$idx$sd
1/res$summary.random$idx$sd^2
