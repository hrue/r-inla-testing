log1exp = function(x, beta,  alpha, gamma)
{
    return (beta * log(1.0 + exp(alpha - gamma * x)))
}

n = 100
lambda = 2
s=0.1
x = rpois(n, lambda = lambda)
beta = 1
alpha = 0
gamma = .5

y = log1exp(x, beta,  alpha, gamma) + rnorm(n, sd = s)
r = inla(y ~ -1 + f(x, model="log1exp"),
        data = data.frame(y, x),
        family = "gaussian",
        control.inla = list(h=0.001), 
        control.family = list(
                hyper = list(
                        prec = list(
                                initial = log(1/s^2),
                                fixed = TRUE))))
summary(r)
