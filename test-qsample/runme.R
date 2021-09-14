n = 3
Q = diag(n)
ns = 2

## sample and evaluate a sample
x = inla.qsample(n, Q=Q, logdens=TRUE)
xx = inla.qsample(Q=Q,  sample = x$sample)
print(x$logdens - xx$logdens)

## use of a constraint
constr = list(A = matrix(1, 1, n), e = 0)
x = inla.qsample(n, Q=Q, constr=constr)
print(constr$A %*% x)

## control the RNG. 
x = inla.qsample(n, Q=Q, seed = 123)
## restart from same seed,  only sample 1
xx = inla.qsample(n=1, Q=Q, seed = 123)
## continue from the save state, sample the remaining 2
xxx = inla.qsample(n=n-1, Q=Q, seed = -1)
## should be 0
print(x - cbind(xx, xxx))




