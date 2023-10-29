
INLA:::inla.my.update()
inla.setOption(num.threads = "1:1", smtp = "taucs", safe = FALSE)

n <- 15
x <- rnorm(n)
xx <- rnorm(n)

eta <- -1 + 0.4 * x + 0.6 * xx
y <- rpois(n, exp(eta))

r <- inla(y ~ 1 + x + xx,
          data = data.frame(y, x, xx),
          family = "poisson",
          control.fixed = list(prec.intercept = 1, prec = 1), 
          control.inla = list(control.vb = list(enable = FALSE)), 
          verbose = TRUE)
r.vb <- inla(y ~ 1 + x + xx,
          data = data.frame(y, x, xx),
          family = "poisson",
          control.fixed = list(prec.intercept = 1, prec = 1), 
          control.inla = list(control.vb = list(enable = TRUE, strategy = "variance")), 
          verbose = TRUE)

Sys.setenv(INLA_NEW_TEST = 1)
rr <- inla(y ~ 1 + x + xx,
          data = data.frame(y, x, xx),
          family = "poisson",
          control.fixed = list(prec.intercept = 1, prec = 1), 
          verbose = TRUE,
          control.inla = list(verbose = TRUE, control.vb = list(enable = !FALSE)), 
          control.mode = list(result = r, restart = TRUE),
          inla.call = "inla.mkl.work",
          num.threads = "1:1", safe = FALSE)

model <- "
    model {
        for(i in 1:N) {
            eta[i] <- beta0 + beta1 * x[i] + beta2 * xx[i];
            y[i] ~ dpois(exp(eta[i]))
        }
        beta0 ~ dnorm(0, 1)
        beta1 ~ dnorm(0, 1)
        beta2 ~ dnorm(0, 1)
    }"
library(runjags)
r.mcmc <- combine.mcmc(run.jags(model = model, data = list(y = y, x = x, xx = xx, N = n),
                                monitor = c("beta0", "beta1", "beta2"), sample = 10^6, 
                                n.chains = 4, method = "parallel"))

r$summary.fixed[,c("mean","sd")]
r.vb$summary.fixed[,c("mean","sd")]
rr$summary.fixed[,c("mean","sd")]
cbind(apply(r.mcmc,2,mean), apply(r.mcmc,2,sd))

