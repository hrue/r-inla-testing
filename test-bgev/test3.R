library(evd)
library(numDeriv)

xi = 1
s = 1

hess.func = function(mu, scale, shape) {
    dgev(1, loc=2, scale=exp(mu), shape = xi, log=TRUE)
    ##dgpd(1, loc=0, scale=exp(mu), shape = xi, log=TRUE)
}

mu = seq(-10, 10, by=0.01)
len = length(mu)
local.prec = numeric(len)
for(i in 1:len) {
    local.prec[i] = -hessian(hess.func, mu[i], scale=s, shape=xi)
}
bad = is.nan(local.prec)
mu = mu[!bad]
local.prec = local.prec[!bad]
neg = (local.prec < 0)
pos = (local.prec >= 0)

plot(mu[pos], local.prec[pos], type = "l",  lwd=4,
     xlim = range(mu), col="green")
lines(mu[neg], local.prec[neg], lwd = 4, col = "red")



    
