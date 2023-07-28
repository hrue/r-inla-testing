## compute predictive data distributions

n <- 20
x <- rnorm(n)
eta <-  1 + 0.2 * x
y <- rpois(n, exp(eta))
y[n] <- NA

r <- inla(y ~ 1 + x,
          data = data.frame(y, x),
          family = "poisson",
          control.compute = list(config = TRUE), 
          control.predictor = list(link = 1))

xx <- inla.posterior.sample(1000, r)
y.pred.fun <- function(idx) {
    rpois(length(Predictor[idx]), exp(Predictor[idx]))
}

yy <- inla.posterior.sample.eval(y.pred.fun, xx, idx = which(is.na(y)))
hist(unlist(yy), prob = TRUE)
