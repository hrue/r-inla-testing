sim.qgamma = function(eta, quantile, s, phi)
{
    shape = s * phi
    q = exp(eta)
    mu = q * shape / qgamma(quantile, shape=shape, rate=1)
    rate = shape / mu
    return (rgamma(n=length(eta), shape = shape, rate = rate))
}
    
n = 1000
scale = rep(1, n)
x = rnorm(n, sd=0.2)
eta = 1.1 + 2.2 * x
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
         num.threads="4:1", 
         verbose = TRUE)

n2 = n %/% 2L
n2NA <- rep(NA, n2 )
Y1 <- inla.surv(c(y[1:n2], n2NA), c(rep(1, n2), n2NA))
Y2 <- inla.surv(c(n2NA, y[n2 + 1:n2]), c(n2NA, rep(1, n2)))
Y <- list(Y1 = Y1, Y2 = Y2)

rr = inla(Y ~ 1 + x, data = list(Y=Y, x=x, scale=scale), 
         family = c("gammasurv","gammasurv"), 
         control.family = list(
             list(
                 control.link = list(
                     model = "quantile", 
                     quantile = quantile)), 
             list(
                 control.link = list(
                     model = "quantile", 
                     quantile = quantile+0.000001))), 
         num.threads="4:1", 
         verbose = TRUE)


n2 = n %/% 2L
n2NA <- rep(NA, n2 )
Y1 <- inla.surv(c(y[1:n2], n2NA), c(rep(1, n2), n2NA))
Y2 <- c(n2NA, y[n2 + 1:n2])
Y <- list(Y1 = Y1, Y2 = Y2)

rrr = inla(Y ~ 1 + x, data = list(Y=Y, x=x, scale=scale), 
         family = c("gammasurv","gamma"), 
         scale = scale, 
         control.family = list(
             list(
                 control.link = list(
                     model = "quantile", 
                     quantile = quantile)), 
             list(
                 control.link = list(
                     model = "quantile", 
                     quantile = quantile+0.000001))), 
         num.threads="4:1", 
         verbose = TRUE)

rr$summary.fixed - r$summary.fixed
rrr$summary.fixed - r$summary.fixed
