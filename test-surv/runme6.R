n <- 10
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
rr=inla.group.cv(r)
## then you'll get mean and stdev of the linear predictor of y_i given y_{-i} and you can work
## it out from this one (assuming its Normal)
with(rr, cbind(mean, sd))

