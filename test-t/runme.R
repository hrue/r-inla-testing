rt.scaled = function(n, dof)
{
    return (rt(n, df = dof) /sqrt(dof/(dof-2)))
}

n=30
dof = 3
x <- rnorm(n)
xx <- scale(arima.sim(n, model = list(ar = 0.9)))
y = 1 + x + xx + 1 * rt.scaled(n, dof)

r = inla(y ~ 1 + x + f(idx, model = "ar1",
                   hyper =  list(prec = list(prior = "pc.prec",
                                             param = c(1, 0.01)),
                                 rho = list(prior = "pc.cor1",
                                            param = c(0.83, 0.5)))), 
        data = data.frame(y, x, idx = 1:n),
        family = "t",
        control.family = list(
                hyper = list(
                        dof = list(
                                prior = "pc.dof",
                                param = c(dof, 0.5)))), 
        verbose=TRUE, 
        control.inla = list(cmin = 0, b.strategy = "keep"))
rr = inla(y ~ 1 + x + f(idx, model = "ar1",
                   hyper =  list(prec = list(prior = "pc.prec",
                                             param = c(1, 0.01)),
                                 rho = list(prior = "pc.cor1",
                                            param = c(0.83, 0.5)))), 
        data = data.frame(y, x, idx = 1:n),
        family = "t",
        control.family = list(
                hyper = list(
                        dof = list(
                                prior = "pc.dof",
                                param = c(dof, 0.5)))), 
        verbose=TRUE, 
        control.inla = list(cmin = Inf, b.strategy = "keep"),
        inla.call = "inla.mkl.work")
