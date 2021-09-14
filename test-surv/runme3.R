n = 1001
x = c()
y = c()
i = 1
low = 1
high = 5
while(i <= n) {
    xx = rnorm(1, sd=0.2)
    eta = 1 + xx
    yy = rexp(1, rate = exp(eta))
    if (yy > low && yy < high) {
        i = i+1
        x = c(x, xx)
        y = c(y, yy)
    }
}

Y = inla.surv(y, event = rep(4, n), truncation = rep(low,n), time2=rep(high, n))

r = inla(Y ~ 1 + x,
         data = list(Y=Y, x=x),
         family = "exponentialsurv",
         control.fixed=list(prec.intercept=1, prec=1))
summary(r)
rr = inla(y ~ 1 + x,
         data = list(Y=Y, x=x),
         family = "exponential",
         control.fixed=list(prec.intercept=1, prec=1))
summary(r)
summary(rr)

