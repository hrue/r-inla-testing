##INLA:::inla.my.update()

n <- 25
x <- rnorm(n, sd = 1)
xx <- rnorm(n, sd = 0.75)
xxx <- rnorm(n, sd = 0.5)

eta <- -1 + 1/sqrt(3) * ( x + xx + xxx )
y <- rpois(n, exp(eta))

rl <- inla(y ~ 1 + x + xx +xxx, 
          data = data.frame(x, xx, xxx, y), 
          family = "poisson",
          control.fixed = list(prec.intercept = 0.1, prec = 0.1), 
          ##inla.call = "inla.mkl.work", 
          control.inla = list(strategy = "laplace"))

ro <- inla(y ~ 1 + x + xx + xxx, 
          data = data.frame(x, xx, xxx, y), 
          family = "poisson",
          control.fixed = list(prec.intercept = 0.1, prec = 0.1), 
          control.inla = list(control.vb = list(strategy = "mean")), 
          inla.mode = "experimental", 
          num.threads = "1:1",
          ##inla.call = "inla.mkl.work", 
          safe = F)
r <- inla(y ~ 1 + x + xx + xxx, 
          data = data.frame(x, xx, xxx, y), 
          family = "poisson",
          control.fixed = list(prec.intercept = 0.1, prec = 0.1), 
          inla.mode = "experimental", 
          num.threads = "1:1",
          control.inla = list(control.vb = list(update.hessian = 10, strategy = "variance", iter.max = 1000)), 
          safe = F, 
          ##inla.call = "inla.mkl.work", 
          verbose = TRUE)

library(runjags)
beta <- combine.mcmc(run.jags(model = "jags.R",
                             data = list(y = y, x = x, xx = xx, xxx = xxx, N = n),
                             monitor = c("intercept", "beta1", "beta2", "beta3"), 
                             n.chains = 4,
                             burnin = 10^3,
                             sample = 10^5, 
                             method = "parallel"))

if (TRUE) {
    INLA:::inla.my.update()
    rr <- inla(y ~ 1 + x + xx + xxx, 
               data = data.frame(x, xx, xxx, y), 
               family = "poisson",
               control.fixed = list(prec.intercept = 0.1, prec = 0.1), 
               inla.mode = "experimental", 
               num.threads = "1:1",
               control.inla = list(control.vb = list(strategy = "variance", iter.max = 1000, hessian.update = 10, hessian.strategy = "full")), 
               safe = F, 
               inla.call = "inla.mkl.work", 
               verbose = TRUE)
}

round(dig = 3,
      cbind(ro = ro$summary.fixed[, "mean"],
            rl = rl$summary.fixed[, "mean"],
            r = r$summary.fixed[, "mean"],
            rr = rr$summary.fixed[, "mean"],
            mcmc = unname(apply(beta, 2, mean))))

round(dig = 3,
      cbind(ro = ro$summary.fixed[, "sd"],
            rl = rl$summary.fixed[, "sd"],
            r = r$summary.fixed[, "sd"],
            rr = rr$summary.fixed[, "sd"],
            mcmc = unname(apply(beta, 2, sd))))

