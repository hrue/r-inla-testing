n = 100
z = runif(n)
zz = runif(n)
z = z -mean(z)
zz = zz -mean(zz)
y = 1+z+zz +rnorm(n, sd=0.1)

formula = y ~ z*zz

lcs = inla.make.lincombs(z=runif(n/2), zz=runif(n/2),  "(Intercept)" = runif(n/2))

r = inla(formula, data = data.frame(y, z, zz), lincomb = lcs, 
        control.fixed = list(correlation.matrix=TRUE))

M = r$model.matrix
C = solve(t(M) %*% (M))
s = sqrt(diag(C))
CC= diag(1/s) %*% C %*% diag(1/s)
rownames(CC) = colnames(CC)  = colnames(M)

for(i in 1:dim(CC)[1]) {
    for(j in 1:dim(CC)[1]) {
        nm = colnames(CC)
        print(c(CC[i, j]-
                r$misc$lincomb.derived.correlation.matrix[nm[i], nm[j]],
                nm[i], nm[j]))
    }
}

