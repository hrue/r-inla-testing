n <- 100
x <- rnorm(n)
pA <- matrix(rnorm(n^2), n, n)
y <- pA %*% (1 + x) + rnorm(n, sd = 0.01)

r <- inla(y ~ 1 + x,
          data = list(y = y, x = x, pA = pA),
          control.compute = list(config = TRUE),
          control.predictor = list(A = pA),
          inla.mode = "experimental")

sam <- inla.posterior.sample(1000, r)

par(mfrow = c(1, 3))

sam.p <- inla.posterior.sample.eval(
  function(pA, x.cov) {
    predictor <- as.numeric(pA %*% (Intercept + x * x.cov))
    return (predictor)
  },
  sam, pA = r$misc$configs$pA, x.cov = x)

lp <- apply(sam.p,1,mean)
plot(lp,  pA %*% (1 + x))

sam.p <- inla.posterior.sample.eval(
  function(pA, x.cov) {
    predictor <- as.numeric(pA %*% Predictor)
    return (predictor)
  },
  sam, pA = r$misc$configs$pA, x.cov = x)

lp <- apply(sam.p,1,mean)
plot(lp,  pA %*% (1 + x))

sam.p <- inla.posterior.sample.eval(
  function(pA, x.cov) {
    predictor <- APredictor
    return (predictor)
  },
  sam, pA = r$misc$configs$pA, x.cov = x)

lp <- apply(sam.p,1,mean)
plot(lp,  pA %*% (1 + x))

