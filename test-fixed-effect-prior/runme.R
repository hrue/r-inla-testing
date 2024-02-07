if (!FALSE) {
    y <- rnorm(1)
    prec <- exp(rnorm(1))
    prior.mean <- rnorm(1)
    prior.prec <- exp(rnorm(1))
} else {
    y <- 1
    prec <- 1
    prior.mean <- 1
    prior.prec <- 3
}
post.prec <- prec + prior.prec
post.mean <- (prior.prec * prior.mean + prec * y) / post.prec

r <- inla(y ~ -1 + xx,
          data = data.frame(y, xx = 1), 
          family = "gaussian",
          control.family = list(hyper = list(prec = list(initial = log(prec), fixed = TRUE))),
          control.fixed = list(mean = prior.mean, prec = prior.prec))
rr <- inla(y ~ 1, 
          data = data.frame(y, xx = 1), 
          family = "gaussian",
          control.family = list(hyper = list(prec = list(initial = log(prec), fixed = TRUE))),
          control.fixed = list(mean.intercept = prior.mean, prec.intercept = prior.prec))

c(true.mean = post.mean, true.sd = sqrt(1/post.prec))
r$summary.fixed[, c("mean", "sd")]
rr$summary.fixed[, c("mean", "sd")]
