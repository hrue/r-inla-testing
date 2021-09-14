n = 100
m = 10 ## m replications with the same frailty
## in total = n*m observations
a = 1
z = runif(n*m)
eta = 1 + z
frailty = rgamma(n, a, a)
y = rexp(n*m,  rate = rep(frailty, each = m) * exp(eta))
event = rep(1, n*m)


## wrong model
r = inla(inla.surv(y, event) ~ 1 + z, data = data.frame(y, event, z),
        family = "exponentialsurv")

## correct model

formula = Y ~ -1 + intercept + zz + 
    f(loggamma.frailty,
      model="iid", hyper = list(prec = list(initial=-5, fixed=TRUE)))

## survival data:
yy = inla.surv( c(y, rep(NA, n)),  c(rep(1, n*m), rep(NA, n)))

## loggamma frailty,  any observation will do, like `1'
ff = c( rep(NA, n*m),  rep(1, n))
Y = list(yy, ff)

intercept = c(rep(1, n*m), rep(NA, n))
zz = c(z,  rep(NA, n))
loggamma.frailty = c(rep(1:n, each=m), 1:n)

rr = inla(formula,  data = list(Y=Y, zz=zz, intercept=intercept,
                            loggamma.frailty= loggamma.frailty),
        ##verbose=TRUE, debug=TRUE, 
        family = c("exponentialsurv",  "loggammafrailty"))

plot(rr$summary.random$loggamma.frailty$mean, log(frailty))
abline(a=0, b=1)
