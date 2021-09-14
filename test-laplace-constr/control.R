lnorm = function(x, mean, S, tol=sqrt(.Machine$double.eps))
{
    eigen.values = eigen(S, symmetric=TRUE)$values
    eigen.values[ eigen.values < tol ] = 0
    non.zero.eigen.values = eigen.values[ eigen.values >0 ]

    q = -0.5 * t(x-mean) %*% my.pinv(S, tol=tol) %*% (x-mean)
    n = length(non.zero.eigen.values)
    lZ = -n/2 * log(2*pi) - 1/2 * sum( log( non.zero.eigen.values ))

    return (as.numeric(lZ + q))
}
cnorm = function(x, mean, S, idx=1, tol = sqrt(.Machine$double.eps))
{
    n2 = length(idx)
    S22 = S[idx,idx, drop=F]
    S21 = S[idx,-idx, drop=F]
    S12 = t(S21)
    S11 = S[-idx,-idx, drop=F]

    SS = S11 - S12 %*% my.pinv(S22, tol=tol) %*% S21

    cm = mean[-idx] + S12 %*% my.pinv(S22, tol=tol) %*% (x[idx]-mean[idx])

    return (list(mean=cm, S=SS))
}

library(Matrix)
Q = read.table("Q.dat", col.names=c("i", "j", "values"))
Q$i = Q$i + 1
Q$j = Q$j + 1

n=6
Q = inla.sparse2dgTMatrix(Q)
A = matrix(scan("A.dat"), 2, 6, byrow=TRUE)
S = as.matrix(solve(Q) - solve(Q) %*% t(A) %*% solve(A %*% solve(Q) %*% t(A)) %*% A %*% solve(Q))
s = sqrt(diag(S))
m = scan("mean.dat")



mp = rep(0,n)
mp[1] = 1
cn = cnorm(mp, rep(0,n), S)
print(cn$mean)
print(sqrt(diag(cn$S)))

