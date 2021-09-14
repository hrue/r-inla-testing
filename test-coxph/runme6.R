n1 = 10^4
stopifnot(n1 %% 100 == 0)
strata1 = rep(1:100, each= n1 %/% 100)
x1 = rnorm(n1, sd=0.2)
rate1 = exp(1+x1)
y1 = rexp(n1, rate=rate1)
event1 = rep(1,n1)
intercept1 = rep(1, n1)
trun <- y1/2 
p1 = inla.coxph(inla.surv(y1, event1, truncation = trun) ~ -1 + intercept1 + x1,
                data = data.frame(y1, event1, x1, intercept1, strata1, trun), 
                control.hazard = list(strata.name = "strata1",
                                      model = "iid", 
                                      diagonal = 0.001, 
                                      hyper = list(prec = list(
                                                       prior = "pc.prec",
                                                       param = c(1, 0.01))), 
                                      n.intervals = 25,
                                      scale.model=TRUE))
r1 = inla(p1$formula, 
          family = p1$family, 
          data=c(as.list(p1$data), p1$data.list), 
          E = E..coxph,
          verbose = TRUE,
          twostage = TRUE)
summary(r1)
