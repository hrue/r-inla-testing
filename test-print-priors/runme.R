n = 20
na = rep(NA, n)
y1 = rnorm(n)
y2 = rbinom(n, size=n, prob = 0.5)
N = 2*n
Y = matrix(NA, N, 2)
Y[1:n, 1] = y1
Y[n + 1:n, 2] = y2

idx = c(1:n, 1:n)
idx2 = c(1:n, n:1)
z = rnorm(N)
zz = rnorm(N)

r = inla(Y ~ 1 + z + zz + f(idx) + f(idx2, model="ar1", group=idx),
         family = list("t",  "binomial"),
         control.family = list(list(), list(control.link = list(model = "sslogit"))), 
         data = list(Y=Y, z=z, zz=zz, idx=idx, idx2=idx2),
         Ntrials = c(na, rep(n, n)), 
         verbose=TRUE, keep=T)

y = rnorm(n)
rr = inla(y ~ 1 + z,
         control.family = list(control.mix = list(model="gaussian")),
         data = data.frame(y, z))



