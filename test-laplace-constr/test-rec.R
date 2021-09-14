library(Matrix)
Q = read.table("Q.dat", col.names=c("i", "j", "values"))
Q$i = Q$i + 1
Q$j = Q$j + 1

n=6
Q = as.matrix(inla.sparse2dgTMatrix(Q))
k = 2
A = matrix(scan("A.dat"), 2, 6, byrow=TRUE)
idx=1

## inv(A*Q^-1*AT)
BB = solve(Q) %*% t(A)
AA = solve( A %*%  BB )

## the constraint-matrix
C.orig = BB %*% AA

## make the change...
one.i = numeric(n)
one.i[idx] = 1
c.i = solve(Q) %*% one.i
v = A %*% c.i
w = AA %*% v
b22 = 1 / as.double( t(one.i) %*% c.i - t(v) %*% w )
C.new = matrix(NA, n, k+1)
C.new[1:n,1:k] = C.orig %*% (diag(k) + b22 * v %*% t(w)) - b22 * c.i %*% t(w)
C.new[1:n,k+1] = b22 * (c.i - BB %*% w)

## testing...
A.new = matrix(NA,k+1,n)
A.new[1:k,1:n] = A
A.new[k+1,1:n] = t(one.i)
C.true = solve(Q) %*% t(A.new)  %*% solve( A.new %*% solve(Q) %*% t(A.new))
dif = C.true - C.new
print(dif)

