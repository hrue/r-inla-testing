n <- 30
m = 50
Z = matrix(runif(n*m), n, m)
if (TRUE) {
    rho = 0.75
    stopifnot(m>1)
    Qz = toeplitz(rho^(0:(m-1))) / (1-rho^2)
} else {
    Qz = diag(m)
    if (m==1 && n==1) {
        Z = matrix(1, 1, 1)
    }
}
z = inla.qsample(1, Q=Qz)
eta = as.vector(Z %*% z)
s = 2
y = eta + rnorm(n, sd = s)
precision=exp(13)
idx = 1:n
fixed=TRUE

r = inla(y ~ -1 +
        f(idx, model="z",  Z=Z, Cmatrix=Qz, param=c(1, 1), precision=precision, initial = 0, fixed=fixed),
        data = list(y=y, idx=idx),
        control.family = list(hyper = list(prec = list(initial = log(1/s^2), fixed=TRUE))), 
        control.predictor = list(compute=TRUE, initial = log(precision), precision = precision), 
        control.compute = list(config=TRUE), 
        control.inla = list(tolerance = 1e-10, int.strategy = "eb"), 
        verbose=TRUE, keep = TRUE)

rr = inla(y ~ -1 +
        f(idx, model="generic",  Cmatrix=Qz, param=c(1, 1), initial=0, fixed=fixed), 
        data = list(y=y, idx=1:m),
        control.family = list(hyper = list(prec = list(initial = log(1/s^2), fixed=TRUE))), 
        control.predictor = list(compute=TRUE, A=Z, initial = log(precision), precision=precision),
        control.inla = list(tolerance = 1e-10, int.strategy = "eb"), 
        verbose=FALSE, keep=TRUE)

Q = r$misc$configs$config[[1]]$Q
d = diag(Q)
Q = Q + t(Q)
diag(Q) = diag(Q) - d
d1 = diag(solve(Q))
d2 = diag(inla.qinv(Q))
mean(abs(d1-d2)/(d1+d2))


## build the Q matrix manually. this |y

kappa.y = 1/s^2
kappa.eta = precision
kappa.v = precision
diag.n = diag(1, n)
zero.n = diag(0, n)
diag.m = diag(1, n)
zero.m = diag(0, n)
zero.nm = matrix(0, n, m)

Q = rBind(
        cBind(kappa.eta * diag.n + kappa.y*diag.n, -kappa.eta * diag.n                  , zero.nm),
        cBind(-kappa.eta * diag.n                , kappa.v * diag.n + kappa.eta * diag.n, -kappa.v * Z),
        cBind(t(zero.nm)                         , -kappa.v * t(Z)                      , kappa.v * t(Z) %*% Z + Qz)
        )
Qinv = inla.qinv(Q)
Qinv2 = solve(Q)
res = cbind(sqrt(diag(Qinv)[(2*n+1):(2*n+m)]), 
      sqrt(diag(Qinv2)[(2*n+1):(2*n+m)]), 
      r$summary.random$idx$sd[-(1:n)], 
      rr$summary.random$idx$sd)
colnames(res) = c("Qinv",  "solve",  "r",  "rr")
rownames(res) = paste("z[", 1:m, "]", sep="")
res = t(res)
print(res)
apply(res,2,range)

## build the Q matrix manually. this joint with y
Qy = rBind(
        cBind(kappa.y*diag.n , -kappa.y*diag.n                    , zero.n                              , zero.nm),
        cBind(-kappa.y*diag.n, kappa.eta * diag.n + kappa.y*diag.n,  -kappa.eta * diag.n                , zero.nm),
        cBind(t(zero.n)      , -kappa.eta * diag.n                , kappa.v * diag.n + kappa.eta * diag.n, -kappa.v * Z),
        cBind(t(zero.nm)     , t(zero.nm)                         , -kappa.v * t(Z)                      , kappa.v * t(Z) %*% Z + Qz))
Sy = as.matrix(solve(Qy)[1:n, 1:n])
Sy <- (Sy + t(Sy))/2
library(mvtnorm)
print(dmvnorm(y, mean = rep(0, n), sigma = Sy,  log=TRUE))

r$summary.random$idx$mean[-(1:n)]- rr$summary.random$idx$mean
r$summary.random$idx$sd[-(1:n)]/rr$summary.random$idx$sd

r$mlik
rr$mlik + 0.5*as.numeric(determinant(Qz, log=TRUE)$modulus)
