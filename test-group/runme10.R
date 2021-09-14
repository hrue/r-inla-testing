prec = matrix(c(2, -2, -2, 5), 2, 2)
covm = solve(prec)
rho = (covm[2, 1] / sqrt(covm[1, 1] * covm[2, 2]))

ng = 3
rr = 0.5
M = toeplitz(c(1+rr^2,  -rr,  rep(0, ng-2)))
M[1, 1] = M[ng, ng] = 1
M = M / (1-rr^2)

eps = 0.000001
Q = kronecker(M, prec)
diag(Q) = diag(Q) + eps

nsamples = 5000
y = c(inla.qsample(nsamples,  Q = Q))

group = rep(rep(1:ng, each = 2), nsamples)
r = rep(1:nsamples, each = dim(Q)[1])
idx = rep(rep(1:2, ng), nsamples)

result = inla(y ~ -1 + f(idx,  model="iid2d", n = 2,
        group = group,
        control.group = list(model = "ar1"), 
        replicate = r),
        data = data.frame(y, idx, group, r),
        family = "gaussian",
        control.family = list(
                hyper = list(
                        prec = list(
                                initial = 10,
                                fixed = TRUE))), 
        verbose=TRUE)
summary(result)


