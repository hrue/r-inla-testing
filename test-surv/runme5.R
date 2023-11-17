n <- 1000
x <- rep(NA, n)
y <- rep(NA, n)
event <- rep(NA, n)
i <- 0
low <- 1
high <- 2
while(i < n) {
    xx <- rnorm(1, sd=0.2)
    eta <- 1 + xx
    yy <- rexp(1, rate = exp(eta))
    if (yy > low) {
        i <- i+1
        x[i] <- xx
        if (yy < high) {
            y[i] <- yy
            event[i] <- 1
        } else {
            y[i] <- high
            event[i] <- 0
        }
    }
}

Y <- inla.surv(y, event = event, truncation = rep(low,n))

r <- inla(Y ~ 1 + x,
         data = list(Y=Y, x=x),
         family = "exponentialsurv",
         verbose = TRUE, 
         control.fixed=list(prec.intercept=1, prec=1))
summary(r)
