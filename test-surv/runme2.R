m=5
time = runif(4*m)
event = rep(0:3, m)
time2 = time+1
truncation = time/2
Y = inla.surv(time, event, time2, truncation)
inla.setOption(num.threads=1)

#################################################
### EXP
#################################################

r = inla(Y ~ 1,  data = list(Y=Y), family = "exponentialsurv")
rr = inla(Y ~ 1,  data = list(Y=Y), family = "exponentialsurv", inla.call = "inla.work")
summary(r)
summary(rr)

r = inla(Y ~ 1,  data = list(Y=Y), family = "gammasurv", verbose=TRUE, keep=T,
         control.fixed = list(prec.intercept = 1))
summary(r)


#################################################
### WEIBULL
#################################################

r = inla(Y ~ 1,  data = list(Y=Y), family = "weibullsurv")
rr = inla(Y ~ 1,  data = list(Y=Y), family = "weibullsurv", inla.call = "inla.work")
summary(r)
summary(rr)

#################################################
### LOGNORMAL
#################################################

r = inla(Y ~ 1,  data = list(Y=Y), family = "lognormalsurv",
         control.family = list(hyper = list(prec =list(initial = 0))))
rr = inla(Y ~ 1,  data = list(Y=Y), family = "lognormalsurv",
          control.family = list(hyper = list(prec =list(initial = 0))), 
          inla.call = "inla.work")
summary(r)
summary(rr)


