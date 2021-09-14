data(Epil)
my.center = function(x) (x - mean(x))

Epil$CTrt    = my.center(Epil$Trt)
Epil$ClBase4 = my.center(log(Epil$Base/4))
Epil$CV4     = my.center(Epil$V4)
Epil$ClAge   = my.center(log(Epil$Age))

gamma.prior = c(1, 0.01)
prec.intercept = 0.001
prec.fixed = 0.001
tau.b1.fixed = exp(1.5)

formula = y ~ ClBase4*CTrt + ClAge + CV4 +
    f(Ind, model="iid", correct=TRUE, 
      hyper = list(prec = list(
                           initial = log(tau.b1.fixed),
                           fixed = TRUE, 
                           prior = "loggamma",
                           param = gamma.prior))) +
    f(rand, model="iid", 
      hyper = list(prec = list(
                           prior = "loggamma",
                           param = gamma.prior)))

result = inla(formula,
    family="poisson",
    data = Epil,
    control.fixed = list(
        prec.intercept = prec.intercept,
        prec = prec.fixed),
    control.predictor = list(compute = TRUE), 
    control.inla = list(
        strategy = "simplified.laplace", 
        int.strategy = "grid",
        diff.logdens = 20,
        dz = 0.25,
        force.diagonal=TRUE, 
        interpolator = "gridsum"),
    verbose=FALSE,
    control.compute = list(config=TRUE))

fnm = "RData.save"
if (file.exists(fnm)) {
    load(fnm)
} else {
    library("runjags")
    mcmc <- run.jags(model = "Epil.jags", 
                     data = list(
                         y = matrix(Epil$y, 59, 4, byrow = TRUE), 
                         log.Base4 = Epil$ClBase4[(1:59)*4],
                         Trt = Epil$CTrt[(1:59)*4],
                         log.Age = Epil$ClAge[(1:59)*4],
                         V4 = Epil$CV4[1:4],
                         prior.a = gamma.prior[1],
                         prior.b = gamma.prior[2],
                         prec.intercept = prec.intercept,
                         prec.fixed = prec.fixed,
                         tau.b1 = tau.b1.fixed, 
                         N = 59,
                         T = 4),
                     burnin = 1000, 
                     n.chains = 10,
                     adapt = 1000,
                     sample=1000, 
                     monitor = c("tau.b", "tau.b1", "a0", "alpha.Age"),
                     method = 'parallel')
    mcmc = INLA:::inla.runjags2dataframe(mcmc)
}


result2 = inla(formula,
    family="poisson",
    data = Epil,
    control.fixed = list(
        correlation.matrix = TRUE,
        prec.intercept = prec.intercept,
        prec = prec.fixed),
    control.predictor = list(compute = TRUE), 
    control.compute = list(config=TRUE), 
    control.inla = list(
        int.strategy = "grid",
        diff.logdens = 20,
        dz = 0.25,
        force.diagonal=TRUE, 
        interpolator = "gridsum",
        correct = TRUE,
        correct.verbose=FALSE, 
        correct.factor = 10), 
    verbose=FALSE)

inla.dev.new()
tau.b = mcmc$tau.b
hist(log(tau.b), prob=TRUE, n=300)
lines(inla.smarginal(result$internal.marginals.hyperpar[[1]]), lwd=2)
dev.copy2eps(file="taub-1.eps")

inla.dev.new()
tau.b = mcmc$tau.b
hist(log(tau.b), prob=TRUE, n=300)
lines(inla.smarginal(result$internal.marginals.hyperpar[[1]]), lwd=2)
lines(inla.smarginal(result2$internal.marginals.hyperpar[[1]]), lwd=2, col="yellow")
dev.copy2eps(file="taub-2.eps")

inla.dev.new()
a0 = mcmc$a0
hist(a0, prob=TRUE, n=300)
lines(inla.smarginal(result$marginals.fixed$'(Intercept)'), lwd=2)
lines(inla.smarginal(result2$marginals.fixed$'(Intercept)'), lwd=2, col="yellow")
dev.copy2eps(file="a0-1.eps")
