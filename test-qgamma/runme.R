sim.qgamma = function(eta, quantile, s, phi)
{
    shape = s * phi
    q = exp(eta)
    mu = q * shape / qgamma(quantile, shape=shape, rate=1)
    rate = shape / mu
    return (rgamma(n=length(eta), shape = shape, rate = rate))
}
    
n = 1000
scale = exp(rnorm(n, sd=1))
x = rnorm(n, sd=0.3)
eta = 1 + 1.2 * x
quantile = 0.95
phi = 2.3

y = sim.qgamma(eta, quantile, scale, phi)

n2 = n %/% 2L
Y = matrix(NA, n, 2)
Y[1:n2, 1] = y[1:n2]
Y[n2 + 1:n2, 2] = y[n2 + 1:n2]

##inla.setOption(smtp='taucs')

r = inla(Y ~ 1 + x, data = list(Y=Y, x=x, scale=scale), 
         scale = scale, 
         family = c("gamma","gamma"), 
         control.family = list(
             list(
                 control.link = list(
                     model = "quantile", 
                     quantile = quantile)), 
             list(
                 control.link = list(
                     model = "quantile", 
                     quantile = quantile+0.000001))), 
         keep=TRUE,
         num.threads=8, 
         verbose = TRUE)
