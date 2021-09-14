## case 1, fewer observations than 'z'
s = 0.00001
n = 100
m = n %/% 2L
z = runif(n)

Y = rep(NA, m)
A = matrix(0,m,m)
for(i in 1:(m-1)) {
    stopifnot(i < m)
    A[i, i:(i+1)] = 1
    Y[i] = sum(1 + z[i:(i+1)]) + rnorm(1, sd=s)
}
Y = c(Y, rep(NA, n-m))
## here, A is automatically expanded to [A, 0; 0, I], where
## dim(I) = n-m 
r = inla(Y ~ 1 + z, control.predictor = list(compute=TRUE,A=A),
        data = data.frame(z,Y),
        control.data = list(initial = log(1/s^2), fixed=T))

## Here, we have twice as many observations than 'z'. then we just
## make new linear predictors, going from 1+z, to 1+zz, where
## zz=rep(z,2).
s = 0.00001
n = 100
fac = 2
m = n*fac
z = runif(n)
zz = rep(z, fac)

Y = rep(NA, m)
A = matrix(0,m,m)
for(i in 1:m) {
    ## just make some random selection
    A[i, sample(1:m, n %/% 10)] = 1
    Y[i] =  sum(A[i,]*(1+zz)) + rnorm(1, sd=s)
}
rr = inla(Y ~ 1 + zz, control.predictor = list(compute=TRUE,A=A),
        data = data.frame(z,Y),
        control.data = list(initial = log(1/s^2), fixed=T))
