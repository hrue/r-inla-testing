kld.sym = function(mean, var) {
    0.5*(
        0.5 * (var[1]/var[2] + (mean[2]-mean[1])^2/var[2] -1 + log(var[2]/var[1]))
        + 
        0.5 * (var[2]/var[1] + (mean[1]-mean[2])^2/var[1] -1 + log(var[1]/var[2]))
        )
}   

prior.param = c(1, 1)

results = c()
for (intercept in seq(-0.5, 0, by = .25)) {
    n = 300
    idx = 1:n
    eta = intercept + rnorm(n)
    y = rpois(n, lambda = exp(eta))
    r = (inla(y ~ 1 + f(idx, param=prior.param),
              data = data.frame(y,idx),
              control.fixed = list(prec.intercept = 1, prec=1), 
              family = "poisson"))
    r = inla.hyperpar(r, diff.logdens = 20, dz = 0.2)
    rr = (inla(y ~ 1 + f(idx, param=prior.param),
               data = data.frame(y,idx),
               family = "poisson",
               control.fixed = list(prec.intercept = 1, prec = 1), 
               control.compute = list(config=TRUE), 
               control.inla = list(
                   correct=TRUE,
                   correct.factor = 100.0, 
                   correct.strategy = "simplified.laplace", 
                   correct.verbose=TRUE)))
    rr = inla.hyperpar(rr, diff.logdens = 20, dz = 0.2)
    library("rjags")
    jags <- jags.model("poisson.jags", 
                       data = list(
                           y = y,
                           prior.a = prior.param[1],
                           prior.b = prior.param[2],
                           prec.intercept = 1,
                           N = length(y)), 
                       n.chains = 1,
                       n.adapt = 1000)
    update(jags, 100)
    mcmc = jags.samples(jags, c("tau", "intercept"), 20000)

    if (TRUE) {
        dev.new()
        par(mfrow=c(2, 1))
        plot(inla.smarginal(r$internal.marginals.hyperpar[[1]]),
             type="l", lwd=3, col = "red", xlab = "log.prec", 
             main = paste("intercept", intercept))
        lines(inla.smarginal(rr$internal.marginals.hyperpar[[1]]),
              lwd=3, col="blue")        
        hist(log(mcmc$tau), prob=TRUE, n=100, add=TRUE)
        plot(inla.smarginal(r$marginals.fixed[[1]]),
             type="l", lwd=3, col = "red", xlab = "intercept", 
             main = paste("intercept", intercept))
        lines(inla.smarginal(rr$marginals.fixed[[1]]),
              lwd=3, col="blue")        
        hist(c(mcmc$intercept), prob=TRUE, n=100, add=TRUE)
    }
    
    xx = cbind(
        mean = c(
            r$internal.summary.hyperpar$mean[1],
            rr$internal.summary.hyperpar$mean[1],
            mean(log(mcmc$tau))),
        sd = c(
            r$internal.summary.hyperpar$sd[1],
            rr$internal.summary.hyperpar$sd[1],
            sd(log(mcmc$tau))))
    rownames(xx) = c("Without", "With", "MCMC")
    print(round(xx, digits=3))

    xx = c(
        "Without" = (sqrt(kld.sym(mean = c(
                                      r$internal.summary.hyperpar$mean[1],
                                      mean(log(mcmc$tau))),
                                  var = c(r$internal.summary.hyperpar$sd[1]^2,
                                      var(log(mcmc$tau)))))),
        "With" = (sqrt(kld.sym(mean = c(
                                   rr$internal.summary.hyperpar$mean[1],
                                   mean(log(mcmc$tau))),
                               var = c(rr$internal.summary.hyperpar$sd[1]^2,
                                   var(log(mcmc$tau)))))))
    print(c(intercept = intercept, round(xx, digits=3),
            ratio = as.numeric(round(xx[2]/xx[1], digits=3))))
    results = rbind(results,
        c(intercept = intercept,
          round(xx, digits=3),
          ratio = as.numeric(round(xx[2]/xx[1], digits=3))))

    ##dev.new();
    ##hist(y, n = n,  main = paste("y with intercept", intercept))

   ##stop("xx")
} 

plot(results[,1], results[,4], ylim=c(0, max(results[, 4])),
     xlab = "intercept", ylab = "KLDsym(With)/KLDsym(Without)")
abline(h=1)
b = coef(lm(results[,4] ~ results[,1]))
lines(results[, 1], b[1] + b[2] * results[, 1])
