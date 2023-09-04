rgp = function(n, sigma, eta, alpha, xi = 0.001)
{
    if (missing(sigma)) {
        stopifnot(!missing(eta) && !missing(alpha))
        sigma = exp(eta) * xi / ((1.0 - alpha)^(-xi) -1.0)
    }
    return (sigma / xi * (runif(n)^(-xi) -1.0))
}

n = 100
x = rnorm(n)
eta = 1+x
alpha = 0.5
xi = 0.4
y = rgp(n, eta = eta, alpha = alpha, xi=xi)

r = inla(y ~ 1+x + f(idx),
         data = data.frame(y, x, idx = 1:n), 
         family = "gp",
         control.family = list(
             control.link = list(quantile = alpha),
             hyper = list(tail = list(
                              prior = "pc.gevtail",
                              param = c(7, 0.25, 0.5)))), 
         control.predictor = list(compute=TRUE),
         control.inla = list(cmin = 0), 
         control.compute = list(config = TRUE), 
         verbose=TRUE)

rr = inla(y ~ 1+x+f(idx),
         data = data.frame(y, x, idx = 1:n), 
         family = "gp",
         control.family = list(
             control.link = list(quantile = alpha),
             hyper = list(tail = list(
                              prior = "pc.gevtail",
                              param = c(7, 0.25, 0.5)))), 
         control.predictor = list(compute=TRUE),
         control.compute = list(config = TRUE),
         inla.call = "inla.mkl.work", 
         verbose=TRUE)
