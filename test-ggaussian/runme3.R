## This is an example about an unconventional use of 'ggaussian'-likelihood, which makes it
## possible to general iid random effects using 'formula' to control the log(precision).

## simulate true data
n <- 300
x <- 1:n
log.prec <- 1 + sin(x * 0.10) * exp(-2*x/n)
u <- rnorm(n, sd = sqrt(1/exp(log.prec)))
eta <- 3 + sin(x * 0.05) * exp(-2*(n-x+1)/n)
y <- rpois(n, lambda = exp(eta + u))

## the aim is to estimate both the smooth eta and the structure in the precision of 'u'
library(dplyr) ## bind_rows

## first we construct fake 0-observations to formulate the iid 'u' and use ggaussian to put the
## linear predictor in the log-precision

## ~ intercept.1 + f(x1, model="rw2", scale.model=TRUE)
y.1 <- rep(0, n)
intercept.1 <- 1
x1 <- 1:n


