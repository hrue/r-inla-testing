n = 10
A = matrix(sample(0:2, size=n^2, replace=TRUE), n, n)
A = inla.as.sparse(A)

m = 1:n
A@x[m] = NA
A = inla.as.sparse(A)
print(A)
A = inla.as.sparse(A, na.rm=TRUE)
print(A)
A = inla.as.sparse(A, zeros.rm=TRUE)
print(A)
