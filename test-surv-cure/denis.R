INLA:::inla.my.update(b = T)


n <- 10^3
pcure <- 0.2
x <- scale(rnorm(n))
xx <- scale(rnorm(n))
eta <- 1 + 0.2 * x
eta.cure <- -1 + 0.2 * xx
pcure <- mean(1/(1+exp(-eta.cure)))
lambda <- exp(eta)
alpha <- 1.2
variant <- 1
y = rweibull(n, shape= alpha, scale= if (variant == 0) lambda^(-1/alpha) else 1/lambda)
time.max <- quantile(y, 0.95)
for(i in 1:n) {
    if (runif(1) < 1/(1+exp(-eta.cure[i]))) {
        y[i] <- time.max+1
    }
}
event <- rep(1, n)
event[y >= time.max] <- 0
Y <- inla.surv(y, event = event, cure = cbind(1, xx))
r <- inla(Y ~ 1 + x,
          data = list(Y = Y, x = x),
          family = "weibullsurv",
          control.family = list(
              variant = variant,
              hyper = list(beta1 = list(prior = "normal",
                                        param = c(0, 1)),
                           beta2 = list(prior = "normal",
                                        param = c(0, 10)))), 
          control.fixed = list(prec = 1, prec.intercept = 1), 
          control.inla = list(cmin = 0.0, control.vb = list(emergency = 25)), 
          verbose = TRUE,
          safe = FALSE)
##r <- inla.rerun(r)
summary(r)
