n = 100
nz = 10
Qz = matrix(runif(nz^2)-0.5, nz, nz)
Qz = Qz %*% t(Qz)
beta = inla.qsample(n=1, Qz)
Z = matrix(rnorm(n*nz), n, nz)


y = Z %*% beta + 0
m = inla.model.wrapper("joint", covariates = Z,  Q = Qz,
        hyper = list(prec = list(fixed=FALSE)))
i = 1:n
r = inla(y ~ f(i, model=m), data = data.frame(y, i))

## then these are the regression coof
print(r$summary.random$i$mean[n+1:nz] - beta)

## and the first 'n' are their sum
print(r$summary.random$i$mean[1:n])





