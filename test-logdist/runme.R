logdist = function(x, beta,  alpha)
{
    return (beta * (1 + exp(alpha[1] * log(x) - alpha[2] * x)))
}

n = 1000
s=0.1
x = runif(n)
beta = 1
alpha = c(1, 0.5)
## start at the true values
hyper = list(
    beta = list(initial = beta), 
    a1 = list(initial = log(alpha[1])),
    a2 = list(initial = log(alpha[2])))
## start somewhere else
hyper = list(
    beta = list(initial = 1), 
    a1 = list(initial = 0), 
    a2 = list(initial = 0))

y = logdist(x, beta,  alpha) + rnorm(n, sd = s)
r = (inla(y ~ -1 + f(x, model="logdist", hyper = hyper), 
          data = data.frame(y, x),
          family = "gaussian",
          verbose=TRUE, 
          control.inla = list(h=0.0001), 
          control.family = list(
              hyper = list(
                  prec = list(
                      initial = log(1/s^2),
                      fixed = TRUE)))))
summary(r)
