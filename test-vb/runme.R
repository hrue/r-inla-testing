INLA:::inla.my.update(b = T)
set.seed(123)
n <- 10
##y <- rbinom(n, size = 1, prob = 0.8)
##family <- "binomial"
y <- rpois(n, 1)
family <- "poisson"
vb.correct <- TRUE
r <- inla(y ~ 1, data = data.frame(y = y),
          family = family, 
          ##control.family = list(hyper =  list(prec = list(initial = 0, fixed = TRUE))), 
          control.inla = list(control.vb = list(enable = vb.correct),
                              int.strategy = 'eb', 
                              strategy = "gaussian"), 
          verbose = TRUE,
          keep = TRUE, 
          control.predictor = list(compute = TRUE), 
          num.threads = 1)
rr <- inla(y ~ 1, data = data.frame(y = y),
          family = family, 
          ##control.family = list(hyper =  list(prec = list(initial = 0, fixed = TRUE))), 
          control.inla = list(control.vb = list(enable = !vb.correct),
                              int.strategy = 'eb', 
                              strategy = "simplified.laplace"), 
          control.predictor = list(compute = TRUE), 
          num.threads = 1)
rrr <- inla(y ~ 1, data = data.frame(y = y),
          family = family, 
          ##control.family = list(hyper =  list(prec = list(initial = 0, fixed = TRUE))), 
          control.inla = list(control.vb = list(enable = !vb.correct),
                              int.strategy = 'eb', 
                              strategy = "laplace"), 
          control.predictor = list(compute = TRUE), 
          num.threads = 1)

print(c(g = r$summary.fixed[, "mean"], 
        sla = rr$summary.fixed[, "mean"], 
        la = rrr$summary.fixed[, "mean"]))
print(c("la-g" = rrr$summary.fixed[, "mean"]-r$summary.fixed[, "mean"], 
        "la-sla" = rrr$summary.fixed[, "mean"]-rr$summary.fixed[, "mean"]))

plot(inla.smarginal(rrr$marginals.fixed[[1]]), lwd = 13, type = "l")
lines(inla.smarginal(rr$marginals.fixed[[1]]), lwd = 3, col = "red")
lines(inla.smarginal(r$marginals.fixed[[1]]), lwd = 3, col = "blue")


m <- seq(-10, 10, by = 0.001)
d <- numeric(length(m))
for(i in 1:length(m)) {
    if (family == "poisson") {
        d[i] <- sum(dpois(y, lambda = exp(m[i]), log = TRUE))
    } else {
        d[i] <- sum(dbinom(y, size = 1, prob = 1/(1+exp(-m[i])), log = TRUE))
    }
}
d <- exp(d-max(d))
d <- d/sum(d) / diff(m)[1]
lines(m, d, lwd = 2, col = "green")
