n = 500
h = 0.01
x = runif(n) - 0.5
lambda = exp(1 + x)
N = rpois(n, lambda)
xx = runif(n) - 0.5
intercept = 1
eta = intercept + xx
p = exp(eta)/(exp(eta) + 1)
y = rbinom(n, size = N, prob = p)

Y = inla.mdata(y=y, cbind(1, x))
r = inla(Y ~ -1 + offset(off) + xx,
         data = list(Y=Y, xx=xx, off=rep(intercept, n)),
         family = "nmix",
         control.fixed = list(prec.intercept=1,  prec=1),
         control.inla = list(cmin=0, h=h, tolerance = 1e-10), 
         verbose=TRUE,
         keep=TRUE)
rr = inla(Y ~ -1 + offset(off) + xx,
         data = list(Y=Y, xx=xx, off=rep(intercept, n)),
         family = "nmix",
         control.fixed = list(prec.intercept=1,  prec=1),
         control.inla = list(h=h, tolerance=1e-10), 
         verbose=TRUE)
rr = inla.rerun(rr)

