inla.my.update(bin=TRUE)
##inla.setOption("num.threads",  4)
##inla.setOption("inla.call", "inla")

n = 500
m = 2
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
        control.data=list(initial=10,fixed=TRUE), keep=TRUE, verbose=T)

r = inla(formula, data = data.frame(i,y),
        control.data=list(initial=10,fixed=TRUE), keep=TRUE, verbose=T,
        control.mode = list(result = r,  restart = TRUE))

r = inla(formula, data = data.frame(i,y),
        control.data=list(initial=10,fixed=TRUE), keep=TRUE, verbose=T, 
        control.mode = list(result = r,  restart=TRUE),
        control.inla = list(h = 1e-4))

rr = inla(formula, data = data.frame(i,y),
        control.data=list(initial=10,fixed=TRUE), keep=TRUE, verbose=T, 
        control.inla = list(
                numint.relerr = 1e-8,
                numint.abserr = 1e-8, 
                interpolator = "ccdintegrate", 
                numint.maxfeval = 10000000,
                h = 1e-4), 
        control.mode = list(result = r,  restart=TRUE))

##print(summary(r))
##print(1/diag(cov(yy)))
##print(cor(yy))

for(j in 1:length(r$internal.marginals.hyperpar)) {
    dev.new()
    plot(rr$internal.marginals.hyperpar[[j]])
    aa = inla.emarginal(function(x) c(x, x^2), rr$internal.marginals.hyperpar[[j]])
    print(paste("integral", aa[2]-aa[1]^2))
    
    lines(r$internal.marginals.hyperpar[[j]])

    aa = inla.emarginal(function(x) c(x, x^2), r$internal.marginals.hyperpar[[j]])
    print(paste("approx", aa[2]-aa[1]^2))
    title(paste("hyper", j, "int:dotted, approx:line"))
}
