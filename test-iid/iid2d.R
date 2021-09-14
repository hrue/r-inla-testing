n = 1000
N = 2*n
rho = 0.5
## set variances
Sigma = matrix(c(1/1, NA, NA, 1/2), 2, 2)
## and the correlation
Sigma[1,2] = Sigma[2,1] = rho*sqrt(Sigma[1,1]*Sigma[2,2])

## need it to simulate data
library(mvtnorm)

if (TRUE)
{
    ## first example
    
    y = yy = rmvnorm(n, sigma=Sigma)
    y = c(y[,1], y[,2])

    i = 1:N
    formula = y ~ f(i, model="iid2d", n=N)

    r = inla(formula, data = data.frame(i,y),
            control.family=list(initial=10,fixed=TRUE))
    print(summary(r))

    print(1/diag(cov(yy)))
    print(cor(yy)[1,2])
}

if (FALSE)
{
    ## second example

    y = yy = rmvnorm(n, sigma=Sigma)
    z = rnorm(n)
    zz = rnorm(n)
    y = y[,1] + z*y[,2] + zz
    i = 1:n
    j = n + 1:n
    formula = y ~ f(i, model="iid2d", n=N) + f(j,z,copy="i") + zz 

    r = inla(formula, data = data.frame(i,j,y,z,zz),
            control.family=list(initial=10,fixed=TRUE),keep=T)
    print(summary(r))
    print(1/diag(cov(yy)))
    print(cor(yy)[1,2])
}
