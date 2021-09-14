n = 100
m = 2
intercept = 3
sd.u = 1

y = c()
idx.u = c()
i.u = 0
for(i in 1:n) {
    i.u = i.u + 1
    u = rnorm(1, sd = sd.u)
    idx.u = c(idx.u,  i.u)
    eta = intercept + u
    p = inla.link.invlogit(eta)
    y = c(y, rbinom(n=1, size=m, prob = p))
}

r = inla(y ~ 1 + f(idx.u, model="iid",  param=c(1, .1)),
    data = data.frame(y, idx.u),
    family = "binomial",
    Ntrials = m,
    control.inla = list(
        correct = FALSE)
    )
rr = inla(y ~ 1 + f(idx.u, model="iid",  param=c(1, .1)),
    data = data.frame(y, idx.u),
    family = "binomial",
    Ntrials = m,
    control.inla = list(
        correct = TRUE,
        correct.factor = 10,
        correct.strategy = "laplace",
        correct.verbose = TRUE)
    )

r = inla.hyperpar(r)
rr = inla.hyperpar(rr)

        
