inla.setOption(inla.call = "inla.mkl.work")
inla.setOption(inla.mode = "experimental")
inla.setOption(num.threads = "1:1")


n = 1001
x = c()
y = c()
i = 1
low = runif(n)
high = 3+runif(n)
while(i <= n) {
    xx = rnorm(1, sd=0.2)
    eta = 1 + xx
    yy = rexp(1, rate = exp(eta))
    if (yy > low[i] && yy < high[i]) {
        i = i+1
        x = c(x, xx)
        y = c(y, yy)
    }
}

Y = inla.surv(y, event = rep(0, n), truncation = low)

r = inla(Y ~ 1 + x,
         data = list(Y=Y, x=x),
         family = "exponentialsurv",
         verbose = TRUE, 
         safe = F, 
         control.fixed=list(prec.intercept=1, prec=1))
summary(r)


