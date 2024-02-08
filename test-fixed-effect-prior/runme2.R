library(INLA)

n <- 1000
a <- scale(rnorm(n))
mu <- 0.2 * a
y <- rpois(n, lambda = exp(mu))

data <- tibble(y = y, a = a)

fit1 <- inla(y ~ -1 + a, data = data, family = "poisson")
fit1$summary.fixed[, c("mean", "sd")]

p <- 10^(seq(2, 9, by = 1))
aa.mode <- c()
aa.mean <- c()
aa.sd <- c()
for(prec_a in p) {
    fit2 <- inla(y ~ -1 + a, data = data, family = "poisson", 
                 control.fixed = list(mean = list(a = 0.2), prec = list(a = prec_a)),
                 control.inla = list(int.strategy = "eb", control.vb = list(enable = !FALSE,
                                                                            emergency = 2500)),
                 verbose = T)
    print(fit2$summary.fixed[, c("mean", "sd")])
    aa.mean <- c(aa.mean, fit2$summary.fixed["a", "mean"])
    aa.mode <- c(aa.mode, fit2$mode$x[n+1])
    aa.sd <- c(aa.sd, fit2$summary.fixed["a", "sd"])
}

par(mfrow = c(2, 2))
plot(p, aa.mean, log = "x", main = "mean")
plot(p, aa.mode, log = "x", main = "mode")
plot(p, aa.sd, log = "xy", main = "sd")

if (!FALSE) {
    prec_a <- 1e-5
    fit2 <- inla(y ~ -1 + a, data = data, family = "poisson", 
                 control.fixed = list(mean = list(a = 0.2), prec = list(a = prec_a)),
                 control.inla = list(int.strategy = "eb", control.vb = list(enable = !FALSE, verbose = T, 
                                                                            emergency = 2500)),
                 verbose = T, keep = T)
}


