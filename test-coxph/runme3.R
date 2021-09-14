if (FALSE) {
    n = 6
    strata = c(rep(1, n/2), rep(2, n/2))
    x = runif(n)
    lambda = exp(1+x)
    y = rexp(n, rate=lambda)
    event = rep(1,n)
    data = list(y=y, event=event, x=x)
    y.surv = inla.surv(y, event)
    intercept1 = rep(1, n)

    p = inla.coxph(y.surv ~ -1 + intercept1 + x,
                   data = list(y.surv = y.surv,  x=x, intercept1 = intercept1,
                               strata = strata),
                   control.hazard = list(strata.name = "strata"))
}

nn = 800
sstrata = c(rep(1, nn/2), rep(2, nn/2))
xx = runif(nn)
llambda = exp(1+xx)
yy = rexp(nn, rate=llambda)
eevent = rep(1,nn)
ddata = list(yy=yy, eevent=eevent, xx=xx)
yy.surv = inla.surv(yy, eevent, .special = list(a = 1, b = 1:10))
iintercept1 = rep(1, nn)

pp = inla.coxph(yy.surv ~ -1 + iintercept1 + xx,
                data = list(yy.surv = yy.surv,  xx=xx, iintercept1 = iintercept1,
                            sstrata = sstrata), 
                control.hazard = list(hyper = list(prec = list(fixed=FALSE, initial = -1, prior="loggamma",  param = c(1, 1))),
                                      scale.model=TRUE,
                                      model="rw2", 
                                      strata.name = "sstrata"),
                tag = "1")
rr = inla(pp$formula, 
          family = pp$family, 
          data=c(as.list(pp$data), pp$data.list), 
          E = pp$E)
summary(rr)
