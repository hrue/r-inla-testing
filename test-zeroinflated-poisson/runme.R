n <- 300
x <- rnorm(n, sd = 0.3)
eta <- 2+x
p.zero <- 0.2
y <- numeric(n)

## case 1
for(idx in 1:n) {
    if (runif(1) < p.zero) {
        y[idx] <- 0
    } else {
        y[idx] <- rpois(1, lambda = exp(eta[idx]))
    }
}

r <- inla(y ~ 1 + x,
          data = data.frame(y, x),
          family = "zeroinflatedpoisson1",
          control.predictor = list(compute = TRUE))

## this is approximate correct as the uncertainty in p.zero is not accounted for. to do this,
## one have to rely on Monte Carlo simulations using inla.posterior.sample and
## inla.posterior.sample.eval
inla.dev.new()
p.est <- r$summary.hyperpar[1,"mean"]
plot((1-p.zero)*exp(eta),  (1-p.est) * r$summary.fitted.values$mean, pch = 19)
abline(a = 0, b = 1, lwd = 3)
inla.dev.new()
plot(1:n, (sqrt(y) - sqrt((1-p.est) * r$summary.fitted.values$mean))/0.5)

## case 0
for(idx in 1:n) {
    if (runif(1) < p.zero) {
        y[idx] <- 0
    } else {
        while((y[idx] <- rpois(1, lambda = exp(eta[idx]))) == 0) TRUE
    }
}

r <- inla(y ~ 1 + x,
          data = data.frame(y, x),
          family = "zeroinflatedpoisson0",
          control.predictor = list(compute = TRUE))

## same comment as above
pm <- numeric(n)
for(idx in 1:n) {
    pm[idx] <- inla.emarginal(function(x) exp(x)/(1-exp(-exp(x))), r$marginals.linear.predictor[[idx]])
}

inla.dev.new()
p.est <- r$summary.hyperpar[1,"mean"]
plot((1-p.zero)*exp(eta)/(1-exp(-exp(eta))),  (1-p.est) * pm, pch = 19)
abline(a = 0, b = 1, lwd = 3)

## approximate pearson residual plot
inla.dev.new()
idx <- which(y>0)
plot(idx, (sqrt(y[idx]) - sqrt((1-p.est) * pm[idx]))/0.5)
