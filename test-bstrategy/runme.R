n = 20
size = 1
x = rnorm(n, s=0.2)
eta = 0 + x
sens = 0.9
spec = 0.7
fixed = FALSE
p = 1/(1+exp(-eta))
prob = sens * p + (1-spec)*(1 - p)
y = rbinom(n, size=size,  prob=prob)

a = 100
b = function(s, a) a*(1-s)/s

r = inla(y ~ 1 + x, data = data.frame(y, x, size),
        family = "binomial",
        Ntrials = size,
        verbose = TRUE,
        control.inla = list(
            cmin=0
            ##b.strategy = "skip"
        ), 
        control.family = list(
                control.link = list(
                        model = "sslogit",
                        hyper = list(
                                sens = list(
                                        prior = "logitbeta",
                                        initial = inla.link.logit(sens),
                                        fixed = fixed, 
                                        param = c(a, b(sens, a))),
                                spec = list(
                                        prior = "logitbeta",
                                        initial = inla.link.logit(spec),
                                        fixed = fixed, 
                                        param = c(a, b(spec, a)))))))

