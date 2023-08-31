library(runjags)
library(INLA)
mod <- inla.models()
mod$link$sslogit$status <- NULL
assign("inla.models", mod, env = INLA:::inla.get.inlaEnv())
rm(mod)

n = 300
size = 1
x = rnorm(n, s=0.5)
eta = 1 + x
sens = 0.85
spec = 0.95
p = 1/(1+exp(-eta))
prob = sens * p + (1-spec)*(1 - p)
y = rbinom(n, size=size,  prob=prob)

a = 100
b = function(s, a) a*(1-s)/s

r = inla(y ~ 1 + x, data = data.frame(y, x, size),
        family = "binomial",
        Ntrials = size,
        verbose = TRUE,
        control.fixed = list(prec.intercept = 1, prec = 1), 
        control.inla = list(cmin = 0, b.strategy = "keep",
                            control.vb = list(enable = FALSE)), 
        control.family = list(
                control.link = list(
                        model = "sslogit",
                        hyper = list(
                                sens = list(
                                        prior = "logitbeta",
                                        initial = inla.link.logit(sens),
                                        fixed = TRUE, 
                                        param = c(a, b(sens, a))),
                                spec = list(
                                        prior = "logitbeta",
                                        initial = inla.link.logit(spec),
                                        fixed = TRUE, 
                                        param = c(a, b(spec, a)))))))

Sys.setenv(INLA_VB_FIT = 1)
Sys.unsetenv("INLA_VB_FIT")

rr = inla(y ~ 1 + x, data = data.frame(y, x, size),
        family = "binomial",
        Ntrials = size,
        verbose = TRUE,
        control.fixed = list(prec.intercept = 1, prec = 1), 
        control.inla = list(b.strategy = "keep", control.vb = list(enable = FALSE)), 
        control.family = list(
                control.link = list(
                        model = "sslogit",
                        hyper = list(
                                sens = list(
                                        prior = "logitbeta",
                                        initial = inla.link.logit(sens),
                                        fixed = TRUE, 
                                        param = c(a, b(sens, a))),
                                spec = list(
                                        prior = "logitbeta",
                                        initial = inla.link.logit(spec),
                                        fixed = TRUE, 
                                        param = c(a, b(spec, a)))))),
        inla.call = "inla.mkl.work",
        num.threads = "1:1", safe = FALSE)
        
model <- "
    model {
        for(i in 1:N) {
            eta[i] <- beta0 + beta1 * x[i];
            logit(prob[i]) <- eta[i]
            p[i] <- prob[i] * sens + (1.0 - spec)*(1 - prob[i])
            y[i] ~ dbinom(p[i], Ntrials)
        }
        beta0 ~ dnorm(0, 1)
        beta1 ~ dnorm(0, 1)
    }"
r.mcmc <- combine.mcmc(run.jags(model = model, data = list(y = y, x = x, N = n, Ntrials = size, 
                                                           sens = sens, spec = spec),
                                monitor = c("beta0", "beta1"), sample = 10^5, 
                                n.chains = 4, method = "parallel"))
summary(r)
summary(rr)
summary(r.mcmc)

r$.args$control.inla$control.vb$enable <- TRUE
r$.args$verbose <- FALSE
r <- inla.rerun(r)
summary(r)

rr$.args$control.inla$control.vb$enable <- TRUE
rr$.args$verbose <- FALSE
rr <- inla.rerun(rr)
summary(rr)
