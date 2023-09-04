library(runjags)
library(INLA)
mod <- inla.models()
mod$link$sslogit$status <- NULL
assign("inla.models", mod, env = INLA:::inla.get.inlaEnv())
rm(mod)

n = 50
size = 5
x = rnorm(n, s=1)
eta = 2 + x
sens = 0.7
spec = 0.9
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
        control.inla = list(b.strategy = "keep", cmin = 0, 
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

r.vb = inla(y ~ 1 + x, data = data.frame(y, x, size),
        family = "binomial",
        Ntrials = size,
        verbose = TRUE,
        control.fixed = list(prec.intercept = 1, prec = 1), 
        control.inla = list(b.strategy = "keep", cmin = 0, 
                            control.vb = list(enable = TRUE, strategy = "variance")), 
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

rr = inla(y ~ 1 + x, data = data.frame(y, x, size),
        family = "binomial",
        Ntrials = size,
        verbose = TRUE,
        control.fixed = list(prec.intercept = 1, prec = 1), 
        control.inla = list(b.strategy = "keep", cmin = Inf,
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
                                        param = c(a, b(spec, a)))))),
        inla.call = "inla.mkl.work",
        num.threads = "1:1", safe = FALSE)
        
rr.vb = inla(y ~ 1 + x, data = data.frame(y, x, size),
        family = "binomial",
        Ntrials = size,
        verbose = TRUE,
        control.fixed = list(prec.intercept = 1, prec = 1), 
        control.inla = list(b.strategy = "keep", cmin = Inf,
                            control.vb = list(enable = TRUE, strategy = "variance")), 
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

if (!FALSE) {
    
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
                                    monitor = c("beta0", "beta1"), sample = 10^4, thin = 10, 
                                    n.chains = 4, method = "parallel"))
    s.r <- r$summary.fixed[, c("mean", "sd")]
    s.r.vb <- r.vb$summary.fixed[, c("mean", "sd")]
    s.rr <- rr$summary.fixed[, c("mean", "sd")]
    s.rr.vb <- rr.vb$summary.fixed[, c("mean", "sd")]
    s.mcmc <- summary(r.mcmc)

    for(i in 1:2) {
        if (i == 1) cat("Intercept\n") else cat("beta\n")
        m = s.mcmc$statistics[i, c("Mean",  "SD")][1]
        s = s.mcmc$statistics[i, c("Mean",  "SD")][2]
        print(rbind(r = c(s.r[i, ], err = (s.r[i, 1]-m)/s), 
                    r.vb = c(s.r.vb[i, ], (s.r.vb[i,1]-m)/s), 
                    rr = c(s.rr[i, ], (s.rr[i, 1]-m)/s), 
                    rr.vb = c(s.rr.vb[i, ], (s.rr.vb[i, 1]-m)/s), 
                    mcmc = c(s.mcmc$statistics[i, c("Mean",  "SD")], 0)))
    }

}
