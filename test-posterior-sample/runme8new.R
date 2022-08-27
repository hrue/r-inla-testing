n <- 100
x <- rnorm(n)
ppA <- matrix(rnorm(n^2), n, n)
y <- ppA %*% (1 + x) + rnorm(n, sd = 0.01)

r <- inla(y ~ 1 + x,
          data = list(y = y, x = x, pA = ppA),
          control.compute = list(config = TRUE),
          control.predictor = list(A = pA),
          inla.mode = "experimental")

sam <- inla.posterior.sample(1000, r)

par(mfrow = c(1, 3))

sam.p <- inla.posterior.sample.eval(
  function(x.cov) {
    predictor <- as.numeric(pA %*% (Intercept + x * x.cov))
    return (predictor)
  },
  sam, x.cov = x)

lp <- apply(sam.p,1,mean)
plot(lp,  ppA %*% (1 + x))

sam.p <- inla.posterior.sample.eval(
  function(x.cov) {
    predictor <- as.numeric(pA %*% Predictor)
    return (predictor)
  },
  sam, x.cov = x)

lp <- apply(sam.p,1,mean)
plot(lp,  ppA %*% (1 + x))

sam.p <- inla.posterior.sample.eval(
  function(x.cov) {
    predictor <- APredictor
    return (predictor)
  },
  sam, x.cov = x)

lp <- apply(sam.p,1,mean)
plot(lp,  ppA %*% (1 + x))
