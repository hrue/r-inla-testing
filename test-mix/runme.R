##inla.my.update(binaries=TRUE)

prec.true = 1/4^2
n = 100
Ntrials = 1
u = rnorm(n, sd=1/sqrt(prec.true))
x = rnorm(n, sd=1/sqrt(prec.true))
eta = 1 + x + u
p = exp(eta)/(1+exp(eta))
y = rbinom(n, size=Ntrials,  prob = p)
param = c(1, 0.01)
diff.logdens = 10
dz = 0.2
idx = 1:n

r = inla(
        formula = y ~ 1 + x, 
        data = data.frame(y,idx, x),
        family = "binomial",
        Ntrials = Ntrials,
        control.inla = list(
                strategy = "laplace",
                int.strategy = "grid",
                diff.logdens = diff.logdens,
                dz = dz), 
        control.family = list(
                control.mix = list(
                        model = "gaussian",
                        nq = 21, 
                        hyper = list(prec = list(
                                             initial = log(prec.true),
                                             prior = "loggamma", 
                                             param = param, 
                                             fixed = FALSE)))), 
        keep=TRUE, verbose=TRUE)

##r = inla.hyperpar(r,  diff.logdens=diff.logdens, dz = dz)

rr = inla(
        formula = y ~ 1 + x + f(idx, model="iid", param = param), 
        data = data.frame(y,idx, x),
        family = "binomial",
        Ntrials = Ntrials,
        control.inla = list(
                strategy = "laplace",
                int.strategy = "grid",
                diff.logdens = diff.logdens,
                dz = dz), 
        control.family = list(
                control.mix = list(
                        model = NULL)), 
        keep=FALSE, verbose=TRUE)

##rr = inla.hyperpar(rr,  diff.logdens=diff.logdens, dz = dz)


library(lme4)
rmle = lmer(cbind(y,Ntrials-y) ~ 1 + x + (1|idx), family = binomial(), data = data.frame(idx=idx, x=x), REML=FALSE)

library("rjags")
jags <- jags.model("model.jags", 
                   data = list(
                           y = y,
                           x = x, 
                           N = n,
                           prior.a = param[1],
                           prior.b = param[2],
                           Ntrials = Ntrials), 
                   n.chains = 1,
                   n.adapt = 1000)
update(jags, 1000)
mcmc = jags.samples(jags, c("tau", "intercept", "beta"), 50000)
trace.tau = c(mcmc$tau)
trace.intercept = c(mcmc$intercept)
trace.beta = c(mcmc$beta)

dev.new()
plot(r$marginals.fixed$"(Intercept)",type="l",lwd=2,col="blue", main="intercept")
lines(rr$marginals.fixed$"(Intercept)",type="l",lwd=2,col="red")
hist(trace.intercept, n=100, prob=T, add=T)

dev.new()
plot(r$marginals.fixed$x,type="l",lwd=2,col="blue", main="x")
lines(rr$marginals.fixed$x,type="l",lwd=2,col="red", main="x")
hist(trace.beta, n=100, prob=T, add=T)

dev.new()
plot(r$internal.marginals.hyperpar[[1]],  type="l",  lwd=2,  col="blue", main="log(tau)")
lines(rr$internal.marginals.hyperpar[[1]],  type="l",  lwd=2,  col="red")
hist(log(trace.tau), n=100, prob=T, add=T)
