n = 1000
set.seed(123)

if (FALSE) {
    y = rexp(n)
    r = inla(y ~ 1,  data = data.frame(y), family = "weibull")
}
if (TRUE) {
    y = arima.sim(n, model=list(ar=0.9))
    y = y/sd(y)
    r = inla(y ~ -1 + f(time, model="ar1",
                        hyper = list(rho = list(prior = "pc.cor0", param=c(0.5, 0.5)),
                                     prec = list(prior = "pc.prec", param = c(1, 0.01)))),
             control.family = list(hyper = list(prec = list(fixed=TRUE))), 
             data = data.frame(y, time=1:n))
}
plot(r,  plot.prior=TRUE, single=TRUE, intern=T)
