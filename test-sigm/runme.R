sigm = function(x, halflife, shape = 1)
{
    xx = (x/halflife)^shape
    return (xx/(1.0+xx))
}
revsigm = function(x, halflife, shape = 1)
{
    xx = (x/halflife)^shape
    return (1.0/(1.0+xx))
}


n = 1000
lambda = 10
s=0.01
x = rpois(n, lambda = lambda)
halflife = lambda
shape = 2

y = sigm(x, halflife, shape) + rnorm(n, sd = s)
r = inla(y ~ -1 + f(x, model="sigm"),
        data = data.frame(y, x),
        family = "gaussian",
        control.family = list(
                hyper = list(
                        prec = list(
                                initial = log(1/s^2),
                                fixed = TRUE))))
summary(r)

y = revsigm(x, halflife, shape) + rnorm(n, sd = s)
r = inla(y ~ -1 + f(x, model="revsigm"),
        data = data.frame(y, x),
        family = "gaussian",
        control.family = list(
                hyper = list(
                        prec = list(
                                initial = log(1/s^2),
                                fixed = TRUE))))
summary(r)



  
