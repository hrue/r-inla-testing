nrep = 5
n = 50
y = matrix(NA, n, nrep)
x = c()
xx = c()
intercept = 1

for(i in 1:n) {
    local.x = runif(1) - 0.5
    lambda = exp(2 + local.x)
    N = rpois(1, lambda)
    local.xx = runif(1) - 0.5
    eta = intercept + local.xx
    p = exp(eta)/(exp(eta) + 1)
    nr = sample(1:nrep, 1)
    y[i, 1:nr]= rbinom(nr, size = N, prob = p)
    x = c(x, local.x)
    xx = c(xx, local.xx)
}

Y = inla.mdata(y, 1, x)
r = inla(Y ~ 1 + xx,
         data = list(Y=Y, xx=xx), 
         family = "nmix",
         control.inla = list(cmin = 0), 
         control.fixed = list(prec.intercept=1,  prec=1))

