n = 5000
N = n+1
phi = 0.98
sd.h = 0.4
prec = (1/sd.h)^2
prec.prime = prec / (1-phi^2)
delta = 0.05
param = list(prec.U = 1,
             prec.alpha = 0.01,
             phi.U = 0.9,
             phi.alpha = 0.5,
             prec.delta = 1)

h = numeric(n)
y = numeric(n)
z = numeric(n)
z[n] = NA ## not used
h[1] = rnorm(1, sd = sqrt(1/prec))
y[1] = rnorm(1, sd = exp(h[1]/2))
for(i in 2:n) {
    z[i-1] = if (y[i-1] < 0) 1 else 0
    h[i] = phi * h[i-1] + delta * z[i-1] + rnorm(1, sd = sqrt(1/prec.prime))
    y[i] = rnorm(1, sd = exp(h[i]/2))
}
idx = 1:n

r = inla(y ~ 1 + f(idx, model="ar1c",
                   hyper = list(
                       prec = list(
                           prior = "pc.prec",
                           param = c(param$prec.U, param$prec.alpha)), 
                       rho = list(
                           prior = "pc.cor1",
                           param = c(param$phi.U, param$phi.alpha))), 
                   args.ar1c = list(Z = cbind(z), 
                                    Q.beta = matrix(param$prec.delta, 1, 1))), 
         data = data.frame(y, idx),
         ##inla.mode = "twostage", 
         family = "stochvol")

par(mfrow=c(2, 2))
plot(idx, y, type="l", main = "Data")
plot(inla.tmarginal(function(x) sqrt(1/exp(x)), 
                    r$internal.marginals.hyperpar[[1]]),
     type = "l", lwd=2, main = "Stdev")
abline(v = sd.h)

plot(inla.tmarginal(function(theta) 2*(exp(theta)/(1+exp(theta)))-1, 
                    r$internal.marginals.hyperpar[[2]]),
     type = "l", lwd=2, main = "phi")
abline(v = phi)

## the N'th element is 'delta'
plot(inla.smarginal(r$marginals.random$idx[[N]]), type="l", lwd=2,
     main = "delta")
abline(v = delta)
