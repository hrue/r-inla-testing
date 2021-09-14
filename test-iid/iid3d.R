n = 3000
N = 3*n
rho = c(0.9, 0.7, 0.5)
## set variances
Sigma = matrix(c(1/1, NA, NA, NA, 1/2, NA, NA, NA, 1/3), 3, 3)
## and the correlation
Sigma[1,2] = Sigma[2,1] = rho[1]*sqrt(Sigma[1,1]*Sigma[2,2])
Sigma[2,3] = Sigma[3,2] = rho[2]*sqrt(Sigma[2, 2]*Sigma[3, 3])
Sigma[1,3] = Sigma[3,1] = rho[3]*sqrt(Sigma[1, 1]*Sigma[3, 3])

## need it to simulate data
library(mvtnorm)

if (TRUE)
{
    ## first example
    
    y = yy = rmvnorm(n, sigma=Sigma)
    y = c(y[,1], y[,2], y[, 3])

    i = 1:N
    formula = y ~ f(i, model="iid3d", n=N)

    r = inla(formula, data = data.frame(i,y),
             control.family=list(initial=10,fixed=TRUE), verbose=T,
             control.inla = list(h=0.001))
    r = inla.rerun(r)

    print(summary(r))

    print(1/diag(cov(yy)))
    print(c(cor(yy)[1,2],  rho[1]))
    print(c(cor(yy)[1,3],  rho[3]))
    print(c(cor(yy)[2, 3], rho[2]))
}

if (FALSE)
{
    ## second example

    y = yy = rmvnorm(n, sigma=Sigma)
    z = rnorm(n)
    zz = rnorm(n)
    zzz = rnorm(n)
    y = y[,1] + z*y[,2] + zz*y[, 3]+zzz
    i = 1:n
    j = n + 1:n
    k = 2*n + 1:n
    formula = y ~ f(i, model="iid3d", n=N) + f(j,z,copy="i") + f(k, zz, copy="i")+zzz

    rr = inla(formula, data = data.frame(i,j,k, y,z,zz, zzz),
            control.family=list(initial=10,fixed=TRUE),keep=T)
    print(summary(r))
    print(1/diag(cov(yy)))
    print(cor(yy)[1,2])
    print(cor(yy)[1,3])
    print(cor(yy)[2, 3])
}
