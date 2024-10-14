##    double lambda = PREDICTOR_INVERSE_LINK(x[i] + off);
##    logll[i] = log(prob + (1.0 - prob) * exp(-lambda));


prob <- 0.09
lambda <- function(x) exp(x)
ll <- function(x, prob) log(prob + (1-prob) * exp(-lambda(x)))
xx <- seq(-5, 5, by = 0.01)
yy <- ll(xx, prob)

fun <- splinefun(xx, yy)

par(mfrow = c(1, 2))
plot(xx, fun(xx), type = "l")
plot(xx, fun(xx, deriv = 2), type = "l")
