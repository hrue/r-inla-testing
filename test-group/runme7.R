set.seed(123)
n = 10
y = arima.sim(n, model = list(ar=0.9))
cyclic = FALSE

idx = rep(1, n)
g = 1:n
formula = y ~ -1 + f(idx,  model="iid", group=g,
        initial = 1.1, 
        hyper = list(prec = list(param = c(1, 1))), 
        control.group = list(model = "ar1", param = c(1, 1),
                initial = 1.1, cyclic = cyclic))

r = inla(formula,  data = data.frame(y, idx, g), verbose=TRUE,
        control.inla = list(int.strategy = "eb"), 
        control.family = list(hyper = list(prec = list(initial = 1.1, param=c(1, 1)))))



idx = 1:n
formula = y ~ -1 + f(idx,  model="ar1", cyclic = cyclic,
        hyper = list(prec = list(initial = 1.1, param = c(1, 1)),
                rho = list(initial = 1.1, param = c(1, 1))))

rr = inla(formula,  data = data.frame(y, idx), verbose=TRUE,
        control.inla = list(int.strategy = "eb"), 
        control.family = list(hyper = list(prec = list(initial = 1.1, param=c(1, 1)))))

plot(r$marginals.hyperpar[[3]])
lines(rr$marginals.hyperpar[[3]])

r$mlik
rr$mlik
