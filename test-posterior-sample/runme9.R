n <- 300
x <- rnorm(n, sd = 0.3)
eta <- 1 + x
prob <- 1/(1+exp(-eta))
size <- 1
y <- rbinom(n, size = size, prob = prob)

r <- inla(y ~ 1 + x,
          data = data.frame(y, x),
          control.compute = list(config = TRUE),
          Ntrials = size,
          family = "binomial")

fun.sim <- function(size) {
    n <- length(Predictor)
    prob <- 1/(1+exp(-Predictor))
    y <- rbinom(n, size = size, prob = prob)
    return (mean(y))
}

xx <- inla.posterior.sample(1000, r)
yy <- unlist(inla.posterior.sample.eval(fun.sim, xx, size = size))

print(c(p = mean(prob), p.est = mean(yy)))

