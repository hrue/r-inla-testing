##inla.setOption("inla.call",  "inla.work")
##inla.my.update()

n = 500
m = 5
N = m*n
rho = 0.8
## set variances
Sigma = matrix(NA, m, m)
diag(Sigma) = 1/(1:m)
for(i in 1:m) {
    if (i+1 <= m) {
        for (j in (i+1):m) {
            Sigma[i, j] = Sigma[j, i] = rho * sqrt(Sigma[i, i]*Sigma[j, j])
        }
    }
}

        
## need it to simulate data
library(mvtnorm)

## first example
    
yy = rmvnorm(n, sigma=Sigma)
y = c()
for(i in 1:m)
    y = c(y, yy[, i])

i = 1:N
formula = y ~ f(i, model=paste("iid", m, "d", sep=""), n=N) -1

r = inla(formula, data = data.frame(i,y),
        control.family=list(initial=10,fixed=TRUE), keep=TRUE, verbose=T)

r = inla(formula, data = data.frame(i,y),
        control.family=list(initial=10,fixed=TRUE), keep=TRUE, verbose=T,
        control.mode = list(result = r,  restart = TRUE))

r = inla(formula, data = data.frame(i,y),
        control.family=list(initial=10,fixed=TRUE), keep=TRUE, verbose=T, 
        control.inla = list(numint.relerr = 1e-9,
                numint.abserr = 1e-9,
                numint.maxfeval = 10000000, h=0.001),
        control.mode = list(result = r,  restart=TRUE))

print(summary(r))
print(1/diag(cov(yy)))
print(cor(yy))
