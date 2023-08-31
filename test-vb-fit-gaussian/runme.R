n <- 10
x <- rnorm(n)
xx <- rnorm(n)

eta <- 1 + 0.1 * x + 0.2 * xx
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

Sys.setenv(INLA_VB_FIT = 1)
rr <- inla(y ~ 1 + x + xx,
          data = data.frame(y, x, xx),
          family = "poisson",
          control.fixed = list(prec.intercept = 1, prec = 1), 
          verbose = TRUE,
          control.inla = list(verbose = TRUE, control.vb = list(enable = FALSE)), 
          ##control.mode = list(result = r, restart = TRUE),
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




if (TRUE) {
    m <- -0.399
    s <- sqrt(exp(-0.223))
    xx <- seq(-4, 4, by = 0.01) * s + m

    yy <- dnorm(xx, mean = m, sd = s, log = TRUE)
    plot(xx, yy - max(yy), ylim = c(-10, 2), type = "l", lwd = 3, col = "blue")
    yy <- dpois(y[n], exp(xx), log = TRUE)
    lines(xx, yy - max(yy))

}
