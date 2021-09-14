set.seed(123)
    n = 300
    phi = 0.9
    y = arima.sim(n, model = list(ar = phi))
    y = scale(y)*2
    idx = 1:n
    param.phi = c(0, 1)
    param.prec = c(1, 0.01)
    r1 = inla(y ~ -1 + f(idx, model='ar1',
            hyper = list(
                    prec = list(initial = 1, fixed=FALSE, prior = "loggamma", param = param.prec),
                    rho = list(initial = 3, fixed=FALSE, prior = "gaussian", param = param.phi))),
            family = "gaussian", 
            verbose = T, 
            control.family = list(initial = 5, fixed=TRUE), 
            data = data.frame(y, idx))

    r = inla(y ~ -1 + f(idx, model='ar',
            order = 1, 
            hyper = list(
                    prec = list(initial = 1, fixed=FALSE, prior = "loggamma", param = param.prec),
                    theta2 = list(initial = 3, fixed=FALSE, prior = "gaussian", param = param.phi))), 
            family = "gaussian", 
            verbose = T, 
            control.family = list(initial = 5, fixed=TRUE), 
            data = data.frame(y, idx)) 
    expect_true(all(abs(r1$summary.hyperpar[, "mean"] - r$summary.hyperpar[, "mean"]) < 0.001))
