n = 100
y = arima.sim(n, model=list(ar=0.9))
c.family = list(hyper = list(prec = list(initial = 15, fixed=TRUE)))

idx = 1:n
i = rep(1, n)
data = data.frame(y, idx, i)

prec.fixed = sample(c(TRUE, FALSE), 1)
rho.fixed =  sample(c(TRUE, FALSE), 1)

r = inla(y ~ -1 + f(idx, model="ar1",
        hyper = list(
                rho = list(
                        fixed = rho.fixed,
                        initial = 1, 
                        prior = "gaussian",
                        param = c(0, 10)),
                prec = list(
                        initial = 1, 
                        fixed = prec.fixed, 
                        prior = "loggamma",
                        param = c(10, 1)))),
        data = data,
        control.inla = list(tolerance = 1e-12, int.strategy="eb"), 
        num.threads = 1, 
        family = "gaussian",
        control.family = c.family)
r = inla.rerun(r)

rr = inla(y ~ -1 + f(i, model="iid",
        hyper = list(
                prec = list(
                        initial = 1, 
                        fixed = prec.fixed, 
                        prior = "loggamma",
                        param = c(10, 1))),
        group = idx,
        control.group = list(
                model="ar1", 
                hyper = list(
                        rho = list(
                                initial = 1, 
                                fixed = rho.fixed, 
                                prior = "gaussian",
                                param = c(0, 10))))), 
        data = data,
        control.inla = list(tolerance = 1e-12, int.strategy="eb"), 
        num.threads=1, 
        family = "gaussian",
        control.family = c.family)
rr = inla.rerun(rr)


r$mlik - rr$mlik
prec.fixed
rho.fixed
