inla.my.update(binaries=TRUE)

prec.true = 1/.5^2
n = 100
Ntrials = 1
u = rnorm(n, sd=1/sqrt(prec.true))
x = rnorm(n, sd=1/sqrt(prec.true))
eta = 1 + x + u
p = exp(eta)/(1+exp(eta))
y = rbinom(n, size=Ntrials,  prob = p)
param = c(1, 0.01)
idx = 1:n

result.old = inla( y ~ 1 + x + f(idx, model="iid", param = param),
        data =data.frame(y,idx, x),
        family = "binomial", Ntrials = Ntrials,
        control.inla = list(
                strategy = "laplace",
                int.strategy = "grid",
                diff.logdens = 20,
                dz = 0.1), 
        inla.call = inla.call.builtin(), 
        verbose=F)

result.new = inla(
        formula = y ~ 1 + x, 
        data = data.frame(y,idx, x),
        family = "binomialre",
        Ntrials = Ntrials,
        control.inla = list(
                strategy = "laplace",
                int.strategy = "grid",
                diff.logdens = 20,
                dz = 0.1), 
        control.family = list(
                hyper = list(prec = list(
                                     initial = log(prec.true),
                                     prior = "loggamma", 
                                     param = param, 
                                     fixed = FALSE))),
        verbose=T)

result.old = inla.hyperpar(result.old,  diff.logdens=20, dz = 0.1)
result.new = inla.hyperpar(result.new,  diff.logdens=20, dz = 0.1)

library(lme4)
r = lmer(cbind(y,Ntrials-y) ~ 1 + x + (1|idx), family = binomial(), data = data.frame(idx=idx, x=x), REML=FALSE)

library("rjags")
jags <- jags.model("jags.model", 
                   data = list(
                           y = y,
                           x = x, 
                           N = n,
                           prior.a = param[1],
                           prior.b = param[2],
                           Ntrials = Ntrials), 
                   n.chains = 4,
                   n.adapt = 10000)
update(jags, 1000)
mcmc = jags.samples(jags, c("tau", "intercept", "beta"), 1000000)
trace.tau = c(mcmc$tau)
trace.intercept = c(mcmc$intercept)
trace.beta = c(mcmc$beta)

dev.new()
plot(result.new$marginals.fixed$"(Intercept)",type="l",lwd=2,col="blue", main="intercept")
lines(result.old$marginals.fixed$"(Intercept)",type="l",lwd=2,col="red")
hist(trace.intercept, n=100, prob=T, add=T)

dev.new()
plot(result.new$marginals.fixed$x,type="l",lwd=2,col="blue", main="x")
lines(result.old$marginals.fixed$x,type="l",lwd=2,col="red")
hist(trace.beta, n=100, prob=T, add=T)

dev.new()
plot(result.new$internal.marginals.hyperpar[[1]],  type="l",  lwd=2,  col="blue", main="log(tau)")
lines(result.old$internal.marginals.hyperpar[[1]],  type="l",  lwd=2,  col="red")
hist(log(trace.tau), n=100, prob=T, add=T)
