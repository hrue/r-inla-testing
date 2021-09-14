rgev = function(n=1, xi = 0, mu = 0.0, sd = 1.0) {
    u = runif(n)
    if (xi == 0) {
        x = -log(-log(u))
    } else {
        x = ((-log(u))^(-xi) - 1.0)/xi
    }
    return (x*sd + mu)
}

n = 1000
xi = -0.5
xi.scale = 0.1
y = 1+rgev(n, xi=xi)

formula = y ~ 1 
data = data.frame(y)

r = inla(formula, data = data, family = "gev", verbose=TRUE, 
        control.fixed = list(prec.intercept = 1), 
        control.family = list(gev.scale.xi = xi.scale,
                hyper = list(
                        prec = list(initial = 4,
                                param = c(10, 1)),
                        gev = list(
                                initial = 0,
                                prior = "gaussian", 
                                param = c(0, 100)))))
