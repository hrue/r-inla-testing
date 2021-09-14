n = 10000
x = runif(n)
xx = runif(n)
eta = 2 + xx
beta = runif(1)
lambda = exp(eta) * ( (1-x) + x * exp(beta) )
y = rpois(n, lambda = lambda)
formula = y ~ 1 + xx
r = inla(formula,
        data = data.frame(y, xx, x),
        family = "poisson",
        control.family = list(
                control.link = list(
                        model = "special2",
                        hyper = list(
                                beta = list(
                                        prior = "normal",
                                        param = c(0, 1))))), 
        link.covariates = x)

print(c(estimate = r$summary.hyperpar[1, 1], beta.true = beta))


