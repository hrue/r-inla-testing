y <- rnorm(1)
prec <- exp(rnorm(1))
prior.mean <- rnorm(1)
prior.prec <- exp(rnorm(1))

post.prec <- prec + prior.prec
post.mean <- (prior.prec * prior.mean + prec * y) / post.prec

r <- inla(y ~ -1 + xx,
          data = data.frame(y = y, xx = 1), 
          family = "gaussian",
          control.expert = list(disable.gaussian.check = TRUE), 
          control.family = list(hyper = list(prec = list(initial = log(prec), fixed = TRUE))),
          control.fixed = list(mean = prior.mean, prec = prior.prec),
          safe = FALSE,
          keep = T, 
          verbose = T)

c(true.mean = post.mean, true.sd = sqrt(1/post.prec))
r$summary.fixed[, c("mean", "sd")]

