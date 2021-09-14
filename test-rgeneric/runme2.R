n = 100
y = rnorm(n)
idx = 1:n
model = inla.rgeneric.define(inla.rgeneric.iid.model, n=n)
formula = y ~ -1 + f(idx, model="rgeneric", rgeneric = model)
r = inla(formula, data = data.frame(y, idx), family = "gaussian",
         control.family = list(hyper = list(prec = list(initial = 5,  fixed=TRUE))))

formula = y ~ -1 + f(idx, model="iid", param = c(1, 1))
rr = inla(formula, data = data.frame(y, idx), family = "gaussian",
         control.family = list(hyper = list(prec = list(initial = 5,  fixed=TRUE))))

print(r$mlik - rr$mlik)


nc = 3
A = matrix(rnorm(nc * n), nc, n)
e = rnorm(nc)
constr = list(A=A, e=e)

formula = y ~ -1 + f(idx, model="rgeneric", rgeneric = model,
                     extraconstr = constr)
r = inla(formula, data = data.frame(y, idx), family = "gaussian",
         control.family = list(hyper = list(prec = list(initial = 5,  fixed=TRUE))))

formula = y ~ -1 + f(idx, model="iid", param = c(1, 1),
                     extraconstr = constr)
rr = inla(formula, data = data.frame(y, idx), family = "gaussian",
         control.family = list(hyper = list(prec = list(initial = 5,  fixed=TRUE))))

print(r$mlik - rr$mlik)
