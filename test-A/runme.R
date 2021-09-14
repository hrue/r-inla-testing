n = 100
z = rnorm(n)
zz = 1:n
y = 1 + z + zz + rnorm(n, sd = 0.1)
r = sample(1:n, n)
Y = y[r]
A=sparseMatrix(i=1:n, j=r, x=rep(1,n))
AA = as.matrix(A)

formula = Y ~ z + zz
r = inla(formula, data = data.frame(Y,z,zz),
        control.predictor = list(compute=T, A = A))
rr = inla(formula, data = data.frame(Y,z,zz),
        control.predictor = list(compute=T, A = AA))
