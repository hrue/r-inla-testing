n = 100
z = runif(n)
eta = 1 + 0.1*z
N = 10


## logit
p = exp(eta)/(1+exp(eta))
y = rbinom(n,  size = N, prob = p)
y[(n-10):n] = NA
r = inla(y ~ 1 + z,  data = data.frame(y, z), family = "binomial", Ntrials = rep(N, n),
    control.family = list(control.link = list(model = "logit")),
    control.predictor = list(compute=TRUE))

rr = inla(y ~ 1 + z,  data = data.frame(y, z), family = "binomial", Ntrials = rep(N, n),
    control.family = list(control.link = list(model = "logit")),
    control.predictor = list(compute=TRUE, link=1))

rrr = inla(y ~ 1 + z,  data = data.frame(y, z), family = "binomial", Ntrials = rep(N, n),
    control.family = list(control.link = list(model = "logit")),
    control.predictor = list(compute=TRUE, link=NA))
