n <- 30000
x <- rnorm(n)
eta <- 2 
event <- rep(1, n)
E <- rep(1, n)

offset <- rnorm(n, sd = 0.5)
xx <- rnorm(n)
xxx <- rnorm(n)
eta.c <- offset + 0.3 * xx + 0.5 * xxx

## need two for the censoring
y <- rpois(n, E*exp(eta))
yy <- rpois(n, E*exp(eta))

ylim <- qpois(0.5, (E*exp(eta))[1])

for(i in 1:n) {
    if (y[i] < ylim) {
        event[i] <- 1
    } else {
        y[i] <- ylim
        event[i] <- 0
    }
}
Y <- inla.mdata(y, E, event, offset, xx, xxx)
r <- inla(Y ~ 1,
          data = list(Y = Y, x = x),
          family = "rcpoisson",
          control.family = list(hyper = list(beta1 = list(param = c(0, 1)), 
                                             beta2 = list(param = c(0, 2)))), 
          verbose = !TRUE)
summary(r)
