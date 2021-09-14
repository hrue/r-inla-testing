n = 1000
off = rpois(n, lambda = 1)
x = rnorm(n)
lambda = off + exp(1 + x)
y = rpois(n, lambda = lambda)

r = inla(y ~ 1 + x, data = data.frame(y, x, link.cov = off),
        family = "poisson",
        control.family = list(
                control.link = list(
                        model = "logoffset", 
                        hyper = list(
                                beta = list(
                                        initial = 0, 
                                        ##fixed = TRUE
                                        fixed = FALSE
                                        )))), 
        link.covariates = link.cov)

