make.sm = function(n, prob = 0.1)
{
    A = matrix(runif(n*n), n, n)
    Patt = matrix(runif(n*n), n, n)
    Patt[Patt > prob] = 0
    A = A * Patt
    diag(A) = runif(n)
    A = A %*% t(A)
    A = A / max(diag(A))
    return (inla.as.sparse(A))
}
    
nsim = 100
n = 5
m = 3
Cmat = list()
Q = inla.as.sparse(matrix(0, n, n))
for(i in 1:m) {
    Cmat[[i]] = make.sm(n)
    Q = Q + i*Cmat[[i]]
}

yy = inla.qsample(nsim, Q)
y = c(yy)
idx = rep(1:n, nsim)
r = rep(1:nsim, each = n)

r = inla(y ~ -1 +
    f(idx, model="generic3",
      Cmatrix = Cmat, replicate = r, 
      hyper = list(
          prec1 = list(initial = log(1)), 
          prec2 = list(initial = log(2)),
          prec3 = list(initial = log(3)))), 
    data = list(y=y, Cmat=Cmat, r=r),
    verbose=TRUE, 
    control.family = list(
        hyper = list(
            prec = list(
                initial = 10,
                fixed = TRUE))))
summary(r)


