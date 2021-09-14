n = 100
s = 0.01
y = arima.sim(n = n,  model = list(ar = 0.9)) + rnorm(n, sd=s)
idx = 1:n

rho = seq(0.5, 0.99999, len = 50)
mlik = numeric(length(rho))

to.theta = inla.models()$latent$ar1$hyper$theta2$to.theta
from.theta= inla.models()$latent$ar1$hyper$theta2$from.theta

library(multicore)
mlik = unlist(mclapply(rho,
        function(rho) {
            r = inla(y ~ -1 + f(idx, model = "ar1",
                    hyper = list(rho = list(initial = to.theta(rho),
                                         fixed=TRUE))), 
                    data = data.frame(y, idx),
                    control.data = list(hyper =
                            list(prec = list(initial = log(1/s^2),
                                         fixed = TRUE))))
            rr = inla.hyperpar(r, diff.logdens=200)
            return (rr$mlik[[1]])
        }))

mlik.func = splinefun(rho, mlik - max(mlik))

prior = unlist(lapply(rho,
        function(rho) {
            theta = to.theta(rho)
            param = inla.models()$latent$ar1$hyper$theta2$param
            d = dnorm(theta,  mean = param[[1]], sd = 1/sqrt(param[[2]])) / abs(grad(from.theta, theta))
            return (d)
        }))

post = prior * exp(mlik.func(rho))
post.func = splinefun(rho, post)
## normalize
rrho = seq(min(rho), max(rho),  len = 1000)
z = sum(post.func(rrho)) * diff(rrho)[1]
post.func = splinefun(rrho, post.func(rrho) / z)

## computed using the conditioning approach
plot(rrho,  post.func(rrho),  type = "l")

## standard estimates
r = inla(y ~ -1 + f(idx, model = "ar1"), 
        data = data.frame(y, idx),
        control.data = list(hyper =
                list(prec = list(initial = log(1/s^2),
                             fixed = TRUE))))
lines(r$marginals.hyperpar[[2]], col="blue")

## improved estimates
rr = inla.hyperpar(r, diff.logdens = 200)
lines(rr$marginals.hyperpar[[2]], col="red")
