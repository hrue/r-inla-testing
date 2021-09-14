n = 6
strata = c(rep(1, n/2), rep(2, n/2))
x = runif(n)
lambda = exp(1+x)
y = rexp(n, rate=lambda)
event = rep(1,n)
data = list(y=y, event=event, x=x)
y.surv = inla.surv(y, event)
intercept1 = rep(1, n)

rrr = inla(y.surv ~ -1 + intercept1 + x,
        data = list(y.surv = y.surv,  x=x, intercept1 = intercept1, strata = strata),
        control.hazard = list(strata.name = "strata"),
        family = "coxph")
rrr = inla.hyperpar(rrr)

p = inla.coxph(y.surv ~ -1 + intercept1 + x, list(y.surv = y.surv,  x=x, intercept1 = intercept1, strata = strata),
        control.hazard = list(strata.name = "strata"))

r = inla(p$formula, 
        family = p$family, 
        data=c(as.list(p$data), p$data.list), 
        E = p$E)
summary(r)

## joint model
intercept2 = rep(1, n)
y = 1 + x + rnorm(n, sd=0.1)
df = data.frame(intercept2, x, y)

## new need to cbind the data.frames, and then add the list-part of
## the data
df.joint = c(as.list(inla.cbind.data.frames(p$data, df)), p$data.list)
df.joint$Y = cbind(df.joint$y..coxph, df.joint$y)

## merge the formulas, recall to add '-1' and to use the new joint
## reponse 'Y'
formula = update(p$formula, Y ~ intercept2 -1 + .)

rr = inla(formula,
        family = c(p$family, "gaussian"),
        data = df.joint,
        E = df.joint$E)
rr = inla.hyperpar(rr)

