rt.scaled = function(n, dof)
{
    return (rt(n, df = dof) /sqrt(dof/(dof-2)))
}

n <- 30
dof <- 4
x <- rnorm(n)
xx <- scale(arima.sim(n, model = list(ar = 0.9)))
##y = 1 + x + xx + rt.scaled(n, dof)
y = 1 + x + rt.scaled(n, dof)

range(y - (1 + x))

INLA:::inla.my.update()

r = inla(y ~ 1 + x, 
         data = data.frame(y, x, idx = 1:n),
         family = "t",
         control.family = list(
             hyper = list(
                 prec = list(initial = 0,
                             fixed = TRUE), 
                 dof = list(
                     prior = "normal",
                     param = c(log(dof-2)-1/2, 1.0)))), 
         control.fixed = list(prec.intercept = 0.01, prec = 0.01), 
         verbose=TRUE, 
         control.inla = list(cmin = 0,
                             b.strategy = "keep", 
                             hessian.correct.skewness.only = TRUE))

rr = inla(y ~ 1 + x, 
          data = data.frame(y, x, idx = 1:n),
          family = "t",
          control.family = list(
              hyper = list(
                  prec = list(initial = 0,
                              fixed = TRUE), 
                  dof = list(
                      prior = "normal",
                      param = c(log(dof-2)-1/2, 1.0)))), 
          control.fixed = list(prec.intercept = 0.01, prec = 0.01), 
          verbose=TRUE, 
          control.inla = list(cmin = Inf,
                              b.strategy = "keep",
                              hessian.correct.skewness.only = TRUE), 
          inla.call = "inla.mkl.work")

rrr = rr
rrr$.args$control.inla$cmin <- -Inf
rrr <- inla.rerun(rrr)

library(runjags)
model <- 'model {
    for(i in 1:N) {
        y[i] ~ dt(mu[i], tau, dof)
        mu[i] <- a + b * x[i]
    }
    tau <- dof/(dof-2)
    dof <- 2 + exp(theta)
    a ~ dnorm(0, 0.01)
    b ~ dnorm(0, 0.01)
    theta ~ dnorm(log(doftrue-2)-1/2, 1.0)
}'

mcmc <- combine.mcmc(run.jags(
    model,
    data = list(y = y, x = x, N = n, doftrue = dof), 
    monitor = c('a', 'b', 'theta'), 
    sample = 10^4,
    thin = 1, 
    method = "parallel",
    n.chains = 4))
summary(mcmc)
    

hist(mcmc[, 'theta'], n = 300, probability = TRUE)
lines(inla.smarginal(r$internal.marginals.hyperpar[[1]]), lwd = 3, col = "blue")
lines(inla.smarginal(rr$internal.marginals.hyperpar[[1]]), lwd = 3, col = "red")
lines(inla.smarginal(rrr$internal.marginals.hyperpar[[1]]), lwd = 3, col = "green")
abline(v=r$mode$theta, col="blue",lwd=3)
abline(v=rr$mode$theta, col="red",lwd=3)
abline(v=rrr$mode$theta, col="green",lwd=3)
