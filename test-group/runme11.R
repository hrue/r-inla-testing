prec = matrix(1, 1, 1)
covm = solve(prec)

ng = 3
rr = 0.9
M = toeplitz(c(1+rr^2,  -rr,  rep(0, ng-2)))
M[1, 1] = M[ng, ng] = 1
M = M / (1-rr^2)

eps = 0.000001
Q = kronecker(M, prec)
diag(Q) = diag(Q) + eps

nsamples = 10000
y = c(inla.qsample(nsamples,  Q = Q))

group = rep(rep(1:ng), nsamples)
r = rep(1:nsamples, each = dim(Q)[1])
idx = rep(rep(1, ng), nsamples)

result = inla(y ~ -1 + f(idx,  model="iid", 
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



