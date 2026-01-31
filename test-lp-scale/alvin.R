library(tidyverse)
library(INLA)

n <- 20000
m <- 20
set.fixed <- 1
disc <- exp(rnorm(m, mean = 0, sd = 0.1))
disc <- rep(1, m)
z <- scale(rnorm(n))
s <- 0.1

y <- numeric(n * m)
eta <- numeric(n * m)
k <- 1
for(i in seq_len(n)) {
    for(j in seq_len(m)) {
        eta[k] <- disc[j] * (0 + z[i])
        y[k] <- eta[k] + rnorm(1, sd = s)
        k <- k + 1
    }
}
data <- data.frame(y, id = rep(1:n, each = m), lp.idx = rep(1:m, n))

lp.priors <- vector("list", length(disc))
for (k in seq_along(disc)) {
    lp.priors[[k]] <- list(
        prior = "normal",
        param = c(disc[k], 10), 
        initial = disc[k],
        fixed = if (is.element(k, set.fixed)) TRUE else FALSE
    )
}
names(lp.priors) <- paste0("theta", seq_along(disc))

form <- y ~ -1 + f(id, model = "iid", ##constr = TRUE, 
                  hyper = list(prec = list(prior = "loggamma",
                                           param = c(10^2, 10^2))))
r <- inla(formula = form, data = data, family = "gaussian",
          control.family = list(hyper = list(prec = list(initial = log(1/s^2), fixed = TRUE))),
          lp.scale = lp.idx, control.lp.scale = list(hyper = lp.priors),
          control.fixed = list(prec.intercept = 1),
          control.inla = list(int.strategy = "eb", force.diagonal = TRUE))

disc.est <- numeric(m)
disc.est[set.fixed] <- disc[set.fixed]
disc.est[setdiff(1:m, set.fixed)] <- r$summary.hyperpar[-1, "mean"]

graphics.off()
par(mfrow = c(1, 2))
round(dig = 3, cbind(disc = disc, disc.est = disc.est))
plot(cbind(eta, r$summary.linear.predictor$mean * disc.est), main = "lin.pred")
abline(a = 0, b = 1)
plot(z,  r$summary.random$id$mean, main = "z")
abline(a = 0, b = 1)

summary(r)
