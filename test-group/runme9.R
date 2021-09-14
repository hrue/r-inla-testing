prec = matrix(c(2, -2, -2, 3), 2, 2)
covm = solve(prec)
rho = (covm[2, 1] / sqrt(covm[1, 1] * covm[2, 2]))

##g = inla.read.graph("graph-33.dat")
g = inla.read.graph(matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1), 3, 3))
m = -inla.graph2matrix(g)
diag(m) = 0
diag(m) = - apply(m, 1, sum)
ng = dim(m)[1]

eps = 0.0001
Q = kronecker(m, prec)
diag(Q) = diag(Q) + eps

nsamples = 1000
y = c(inla.qsample(nsamples,  Q = Q))

group = rep(rep(1:ng, each = 2), nsamples)
r = rep(1:nsamples, each = dim(Q)[1])
idx = rep(rep(1:2, ng), nsamples)

result = inla(y ~ -1 + f(idx,  model="iid2d", n = 2,
        group = group,
        control.group = list(model = "besag", graph = g), 
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



1/diag(covm)
rho
