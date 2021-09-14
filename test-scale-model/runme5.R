n = 100
x = 1:n
x = x-mean(x)
eta.true = x^3/50^3*4 
y = eta.true+ rnorm(n, sd = 1)

plot(x, y)
lines(x, eta.true)
inla.dev.new()

prec.fixed = 3
std=FALSE
r1 = inla(y ~ -1 + f(idx, model="rw1", initial = prec.fixed,
        fixed=TRUE, scale.model=std, constr = FALSE), 
        data = data.frame(y=y,idx=idx),
        verbose=FALSE,
        control.family = list(hyper = list(prec = list(initial = 0, fixed=TRUE))))

r2 = inla(y ~ -1 + f(idx, model="rw2", initial = prec.fixed,
        fixed=TRUE, scale.model=std, constr=FALSE), 
        data = data.frame(y=y,idx=idx),
        verbose=FALSE,
        control.family = list(hyper = list(prec = list(initial = 0, fixed=TRUE))))
plot(x, y)
lines(x, r1$summary.random$idx$mean)
lines(x, r2$summary.random$idx$mean, lwd=2)
inla.dev.new()

std=TRUE
r1 = inla(y ~ -1 + f(idx, model="rw1", initial =prec.fixed,
        fixed=TRUE, scale.model=std, constr = FALSE), 
        data = data.frame(y=y,idx=idx),
        verbose=FALSE,
        control.family = list(hyper = list(prec = list(initial = 0, fixed=TRUE))))

r2 = inla(y ~ -1 + f(idx, model="rw2", initial = prec.fixed,
        fixed=TRUE, scale.model=std, constr=FALSE), 
        data = data.frame(y=y,idx=idx),
        verbose=FALSE,
        control.family = list(hyper = list(prec = list(initial = 0, fixed=TRUE))))

plot(x, y)
lines(x, r1$summary.random$idx$mean)
lines(x, r2$summary.random$idx$mean, lwd=2)

Q = INLA:::inla.rw(n, order=1, sparse=FALSE)
exp(mean(log(diag(INLA:::inla.ginv(Q)))))

Q = INLA:::inla.rw(n, order=2, sparse=FALSE)
exp(mean(log(diag(INLA:::inla.ginv(Q)))))

