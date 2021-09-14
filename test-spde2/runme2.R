## artificial test-example for the new spde2 interface.

## AR1 precision matrix

##inla.my.update(bin=TRUE)
##inla.setOption("num.threads", 1)

phi = 0.25
n = 1000
prefix = "data/spde2-"

## build the Q-matrix
ii = c()
jj = c()
xx = c()

ii = c(ii,  1:n)
jj = c(jj,  1:n)
xx = c(xx,  c(1, rep(1+phi^2, n-2), 1))

ii = c(ii, 2:n)
jj = c(jj, 1:(n-1))
xx = c(xx, rep(-phi, n-1))

ii = c(ii, 1:(n-1))
jj = c(jj, 2:n)
xx = c(xx, rep(-phi, n-1))

Q = sparseMatrix(i =ii, j =jj,  x = xx, dims=c(n, n))

## diagonal M0 = phi^2,  M2 = 1. just give the structure
M0 = sparseMatrix(i = 1:n, j=1:n, x = c(0, rep(1, n-2), 0))
M2 = sparseMatrix(i = 1:n, j=1:n, x = rep(1, n))
## lower diagonal,  M1 = -phi
M1 = sparseMatrix(i = 2:n, j=1:(n-1), x=rep(-1, n-1),  dims=c(n, n))

M0 = inla.as.dgTMatrix(M0)
M1 = inla.as.dgTMatrix(M1)
M2 = inla.as.dgTMatrix(M2)

theta.1 = -0.5*log(1-phi^2)
theta.2 = log(phi)
theta.3 = 1

## its faster to make the B'matrices non-sparse.
B0 = matrix(cbind(rep(1, n)), n, 1)
B1 = matrix(cbind(rep(2, n)), n, 1)
B2 = matrix(cbind(rep(3, n)), n, 1)


BLC = NULL

for (mat in c("M0",  "M1", "M2",  "B0", "B1",  "B2", "BLC")) {
    mat.eval = INLA:::inla.eval(mat)
    if (!is.null(mat.eval)) {
        INLA:::inla.write.fmesher.file(mat.eval, paste(prefix, mat, sep=""))
    }
}

y = arima.sim(n=n,  model = list(ar=c(phi)))
y = y / sd(y)  ## variance =  1
    
i = 1:n
ii = 1:n
formula = y ~ -1 +
    f(i, model="spde2", n=n, spde2.prefix=prefix,
      spde2.transform = "identity", 
      hyper = list(
          theta1 = list(initial = theta.1, fixed = F, param = c(0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1)), 
          theta2 = list(initial = theta.2, fixed = F),
          theta3 = list(initial = theta.3, fixed = F)))

r = inla(formula, data = data.frame(y, i, ii), verbose=TRUE, keep=TRUE, 
        control.family =list(hyper = list(prec = list(initial = 10,  fixed=TRUE))),
        control.inla=list(verbose=F, h=1e-6))

print(r$summary.hyperpar[,"mean"])
print(c(theta.1, theta.2, theta.3))
