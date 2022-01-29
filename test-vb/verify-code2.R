y <- 1

rr <- inla(y ~ 1, data = data.frame(y = y),
          family = "poisson",
          inla.mode = "experimental", 
          verbose = TRUE, 
          control.fixed = list(prec.intercept = 1), 
          control.predictor = list(compute = TRUE), 
          control.inla = list(strategy = "gaussian",
                              control.vb = list(enable = TRUE)),
          inla.call = "inla.mkl.work")

mu <- -0.128158822656
s <- 0.707106769085 

fun <- function(m) {
    xx <- seq(m-10*s, m+10*s, by = s/100)
    dd <- sum(dnorm(xx, mean = m, sd = s) * (-dpois(y, lambda = exp(xx), log = TRUE)))
    return (dd * diff(xx)[1])
}

h <- 1e-4 * s
print(fun(mu))
print((fun(mu + h) - fun(mu-h))/(2*h))
print((fun(mu + h) - 2*fun(mu) + fun(mu-h))/h^2)
