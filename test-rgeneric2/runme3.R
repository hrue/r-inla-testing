n = 100
s = 0.1
x = rnorm(n)
y = 1 + x + rnorm(n, sd = s)

save.image(file = "tt.save")
model = inla.rgeneric.define(inla.rgeneric.iid.model, n =n, 
    debug=FALSE, R.init = "tt.save")
r2 = inla(y ~ -1 + f(idx, model=model), 
    data = data.frame(y = y, idx = 1:n),
    control.family = list(hyper = list(prec = list(initial = 12, fixed=TRUE))))

model = inla.rgeneric.define(inla.rgeneric.iid.model, n =n,
                             R.init = save.image)
r3 = inla(y ~ -1 + f(idx, model=model), 
    data = data.frame(y = y, idx = 1:n),
    control.family = list(hyper = list(prec = list(initial = 12, fixed=TRUE))))

print(r2$mlik - r3$mlik)
