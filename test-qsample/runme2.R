ns = 100000
n = 10
Q = matrix(runif(n^2), n, n)
diag(Q) = diag(Q) + n

xx = inla.qsample(ns, Q)
apply(xx, 1, mean)

b = 1:n
mu = 100 + b
new.mean = mu + inla.qsolve(Q, as.matrix(b))
xx = inla.qsample(ns, Q, b=b, mu = mu)
apply(xx, 1, mean) - new.mean

nA = 5
stopifnot(nA < n)
constr = list(A = matrix(runif(n*nA), nA, n), 
        e = 100*(1:nA) + runif(nA))
xx = inla.qsample(ns, Q, b=b, mu = mu, constr=constr, logdens=TRUE)
r = constr$A %*% xx$sample - constr$e
mean(abs(r))

xx = inla.qsample(sample = xx$sample, Q=Q, b=b, mu = mu, constr=constr, logdens=TRUE)
mean(abs(xx$logdens - xx$logdens))

