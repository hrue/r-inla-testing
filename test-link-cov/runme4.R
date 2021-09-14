n = 1000
m = 2
link.z = matrix(rnorm(n*m), n, m)
link.beta = matrix((1:m)/2, 1, m)
eta = rep(1, n)
sdlog = .1
y = numeric(n)
for (i in 1:n) {
    lambda = qlnorm(pnorm(eta[i]), meanlog = sum(link.z[i,]*link.beta) - 0.5*sdlog^2, sdlog = sdlog)
    y[i] = rpois(1, lambda = lambda)
}

r = inla(y ~ 1,
        data = list(y=y), 
        family = "poisson",
        control.fixed = list(prec.intercept = 1), 
        control.family = list(
                control.link= list(model = "special1", order = m,
                        hyper = list(prec = list(param = c(1, .01), initial = log(1/sdlog^2), fixed=FALSE)))), 
        control.predictor = list(
                compute=TRUE,
                link = 1),
        control.inla = list(strategy = "gaussian", int.strategy = "grid", verbose=FALSE, cmin = -10000), 
        link.covariates = link.z,
        ##verbose=TRUE, keep=TRUE
        )

summary(r)
