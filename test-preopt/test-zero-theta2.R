nq <- 50
np <- 10^4
n <- np * nq

theta <- rnorm(nq, sd = sqrt(1))
h <- rnorm(np, sd = sqrt(1))

y <- numeric(n)
for(j in 1:np) {
    y[(j-1)*nq + 1:nq] <- rbinom(nq, prob = 1/(1+exp(h[j] - theta)), size = 1)
}

i <- rep(1:np, each = nq)
j <- rep(1:nq, np)

rr <- inla(y ~ -1 +
              f(i, model = "iid", values = 1:np, hyper = list(prec = list(initial = log(1), fixed = TRUE))) +
              f(j, model = "iid", values = 1:nq, hyper = list(prec = list(initial = log(1), fixed = TRUE)), vb.correct = TRUE),
          data = data.frame(y, i, j),
          family = "binomial",
          inla.mode = "experimental", 
          ##inla.call = "remote", 
          control.inla = list(control.vb = list(enable = FALSE)), 
          verbose = TRUE,
          num.threads = "2:4")
r <- inla(y ~ -1 +
              f(i, model = "iid", values = 1:np, hyper = list(prec = list(initial = log(1), fixed = TRUE))) +
              f(j, model = "iid", values = 1:nq, hyper = list(prec = list(initial = log(1), fixed = TRUE)), vb.correct = TRUE),
          data = data.frame(y, i, j),
          family = "binomial",
          inla.mode = "experimental", 
          ##inla.call = "remote", 
          control.inla = list(control.vb = list(enable = TRUE)), 
          ##verbose = TRUE,
          num.threads = "2:4")
par(mfrow = c(1, 2))
plot(sort(h), sort(r$summary.random$i$mean), pch = 19)
abline(a = 0, b = 1)
plot(sort(theta), sort(r$summary.random$j$mean), pch = 19)
abline(a = 0, b = 1)
## this is the relative error in the mean wrt its own stdev
summary((r$summary.random$j$mean-rr$summary.random$j$mean) / rr$summary.random$j$sd)
