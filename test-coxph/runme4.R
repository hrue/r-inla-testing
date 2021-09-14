n1 = 100
stopifnot(n1 %% 4 == 0)
strata1 = rep(1:4, each= n1 %/% 4)
x1 = rnorm(n1, sd=0.2)
rate1 = exp(1+x1)
y1 = rexp(n1, rate=rate1)
event1 = rep(1,n1)
intercept1 = rep(1, n1)
p1 = inla.coxph(inla.surv(y1, event1) ~ -1 + intercept1 + x1,
                data = data.frame(y1, event1, x1, intercept1, strata1), 
                control.hazard = list(strata.name = "strata1",
                                      hyper = list(prec = list(
                                                       prior = "pc.prec",
                                                       param = c(1, 0.01))), 
                                      n.intervals = 125,
                                      scale.model=TRUE))
r1 = inla(p1$formula, 
          family = p1$family, 
          data=c(as.list(p1$data), p1$data.list), 
          E = E..coxph)
r1 = inla.rerun(r1)
summary(r1)

## add a non-surv model
n2 = n1 -1
intercept2 = rep(1, n2)
x2 = rnorm(n2)
y2 = 1 + x2 + rnorm(n2, sd=0.1)
df2 = data.frame(intercept2, y2, x2)
f2 = y2 ~ -1 + intercept2 + x2
r2 = inla(f2, data = df2)
r2 = inla.rerun(r2)
summary(r2)

df2j = as.list(inla.rbind.data.frames(p1$data, df2))
df2j$Y = inla.rbind.data.frames(data.frame(p1$data$y..coxph), data.frame(df2$y2))
f2 = update(p1$formula, Y ~ -1 + intercept2 + x2 + .)
r2j.data = c(df2j, p1$data.list)
r2j = inla(f2,
           family = c(p1$family, "gaussian"),
           data = r2j.data, 
           E = E..coxph)
r2j = inla.rerun(r2j)

## another coxph model
n3 = 100
stopifnot(n3 %% 2 == 0)
x3 = rnorm(n3, sd=0.2)
strata3 = rep(1:2, each = n3 %/% 2)
rate3 = exp(1+x3)
y3 = rexp(n3, rate=rate3)
event3 = rep(1,n3)
intercept3 = rep(1, n3)
df3 = data.frame(y3, x3, intercept3, strata3)
p3 = inla.coxph(inla.surv(y3, event3) ~ -1 + intercept3 + x3,
                data = df3, 
                control.hazard = list(strata.name = "strata3",
                                      hyper = list(prec = list(
                                                       prior = "pc.prec",
                                                       param = c(1, 0.01))), 
                                      n.intervals=15,
                                      scale.model=TRUE),
                tag="3")
## alone
r3 = inla(p3$formula, 
          family = p3$family, 
          data=c(as.list(p3$data), p3$data.list), 
          E = E..coxph)
r3 = inla.rerun(r3)
summary(r3)

## then join all models together. Note that E..coxph is not 'tagged' with tag, since the final
## use is a vector, i.e. inla(..., E=..) requires a vector, not matrix. hence rbind'ing the
## data.frames will give the correct result

df3j = as.list(inla.rbind.data.frames(p1$data, df2, p3$data))
df3j$Y = inla.rbind.data.frames(data.frame(p1$data$y..coxph),
                                data.frame(df2$y2),
                                data.frame(p3$data$y3..coxph))
df3j.data = c(df3j, p1$data.list, p3$data.list)

## I was unable to use 'update' directly. feels like there should be some easier way to do this.
f1c = as.character(p1$formula)
f2c = as.character(f2)
f3c = as.character(p3$formula)
f3j = as.formula(paste("Y ~ ", f1c[3],  "+", f2c[3], "+", f3c[3]))
f3j = update(f3j,  . ~ .) ## remove potential duplicate terms

r3j= inla(f3j,
          family = c(p1$family, "gaussian", p3$family),
          data = df3j.data, 
          E = E..coxph)
r3j = inla.rerun(r3j)

cat("This is the marginal likelihood difference between the joint model and the 3 separate ones\n")
print(r3j$mlik - (r1$mlik + r2$mlik + r3$mlik))
