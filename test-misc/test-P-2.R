n <- 100
x <- rnorm(n)
eta <- 0+0.5*x
y <- rpois(n, exp(eta))

INLA:::inla.my.update(b = TRUE)
inla.setOption(num.threads = 1)

r <- inla(y ~ 1 + x + f(idx),
          family = "poisson",
          data = data.frame(y, x, idx = 1:n),
          control.compute = list(return.marginals.predictor = TRUE), 
          control.predictor = list(cdf = 0),
          verbose = TRUE)

xx <- c()
yy <- c()
for (i in 1:n) {
    tmp <- inla.pmarginal(0, r$marginals.linear.predictor[[i]])
    print(c(i, tmp, r$summary.linear.predictor$"0cdf"[i]))
    xx <- c(xx, tmp)
    yy <- c(yy, r$summary.linear.predictor$"0cdf"[i])
}
plot(xx, yy)
abline(a = 0, b = 1)
