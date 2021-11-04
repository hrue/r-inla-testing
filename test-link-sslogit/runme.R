library(INLA)
inla.models <- get("inla.models", env = INLA:::inla.get.inlaEnv())
inla.models$link$sslogit$status <- NULL
assign("inla.models", inla.models, env = INLA:::inla.get.inlaEnv())
rm(inla.models)

n = 10000
size = 1
x = rnorm(n, s=0.2)
eta = 2 + x
sens = 0.85
spec = 0.995
fixed = TRUE
p = 1/(1+exp(-eta))
prob = sens * p + (1-spec)*(1 - p)
y = rbinom(n, size=size,  prob=prob)

a = 100
b = function(s, a) a*(1-s)/s

r = inla(y ~ 1 + x, data = data.frame(y, x, size),
        family = "binomial",
        Ntrials = size,
        verbose = TRUE,
        inla.mode = "experimental", 
        control.inla = list(cmin = 1, b.strategy = "keep"), 
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

r$.args$control.inla$cmin <- 0
r <- inla.rerun(r)
summary(r)
