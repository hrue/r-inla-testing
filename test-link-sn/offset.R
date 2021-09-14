library(sn)

n <- 5
siz <- 300
alpha0 <- 5

x <-rnorm(n)
eta <- 0 
p <- inla.link.invsn(eta, alpha0)
y <- rbinom(n, size = siz, prob = p)
pc_param <- 10

res_pc =inla(y ~ -1, 
             data =data.frame(x = x, y = y),
             family = "binomial",
             Ntrials = siz,
             control.family =list(
                 control.link =list(
                     model = "sn",
                     hyper = list(alpha =list(
                                      fixed = FALSE,
                                      prior = "pc.sn",
                                      param = pc_param)))))
summary(res_pc)
plot(res_pc, plot.prior = TRUE)
abline(v = alpha0)

m <- res_pc$marginals.hyperpar[[1]]
a.sim <- inla.rmarginal(1000, m)
xx <- seq(-4, 4, by = 0.1)
plot(NA, NA, xlim = range(xx), ylim = c(0, 1))
for(i in seq_along(xx)) {
    lines(xx, inla.link.invsn(xx, a.sim[i]))
}
lines(xx, pnorm(xx), lwd = 5, col = "green")
