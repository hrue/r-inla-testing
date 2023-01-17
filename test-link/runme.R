INLA:::inla.my.update(b = T)
inla.setOption(inla.call = "inla.mkl.work")

n = 1000
z = runif(n) - 0.5
eta = 1 + 1*z
N = 10


## logit
set.seed(123)
p = inla.link.invlogit(eta)
y = rbinom(n,  size = N, prob = p)
r1 = inla(y ~ 1 + z,  data = data.frame(y, z), family = "binomial", Ntrials = rep(N, n),
        control.family = list(control.link = list(model = "logit")), control.predictor = list(compute=TRUE))

## probit
set.seed(123)
p = inla.link.invprobit(eta)
y = rbinom(n,  size = N, prob = p)
r2 = inla(y ~ 1 + z,  data = data.frame(y, z), family = "binomial", Ntrials = rep(N, n),
        control.family = list(control.link = list(model = "probit")), control.predictor = list(compute=TRUE))

## cloglog
set.seed(123)
p = inla.link.invcloglog(eta)
y = rbinom(n,  size = N, prob = p)
r3 = inla(y ~ 1 + z,  data = data.frame(y, z), family = "binomial", Ntrials = rep(N, n),
        control.family = list(control.link = list(model = "cloglog")), control.predictor = list(compute=TRUE))

## loglog
set.seed(123)
p = inla.link.invloglog(eta)
y = rbinom(n,  size = N, prob = p)
r4 = inla(y ~ 1 + z,  data = data.frame(y, z), family = "binomial", Ntrials = rep(N, n),
        control.family = list(control.link = list(model = "loglog")), control.predictor = list(compute=TRUE))

## cauchits
set.seed(123)
p = inla.link.invcauchit(eta)
y = rbinom(n,  size = N, prob = p)
r5 = inla(y ~ 1 + z,  data = data.frame(y, z), family = "binomial", Ntrials = rep(N, n),
        control.family = list(control.link = list(model = "cauchit")), control.predictor = list(compute=TRUE))

## ccloglog
set.seed(123)
p = inla.link.invccloglog(eta)
y = rbinom(n,  size = N, prob = p)
r6 = inla(y ~ 1 + z,  data = data.frame(y, z), family = "binomial", Ntrials = rep(N, n),
        control.family = list(control.link = list(model = "ccloglog")), control.predictor = list(compute=TRUE))

