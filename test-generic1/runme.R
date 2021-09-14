n = 100
## build a structure matrix
Cm = matrix(runif(n^2,min=-1,max=1),n,n)
diag(Cm) = 0
Cm = 0.5*(Cm + t(Cm))
lambda.max = max(eigen(Cm)$values)
## define the precision matrix
beta = 0.9
Q = diag(rep(1,n)) - beta/lambda.max * Cm
Sigma = solve(Q)
#simulate data
require(mvtnorm)
sd = 0.001
z = rnorm(n)
eta = rmvnorm(n=1,sigma = Sigma)
y = c(eta) + sd*rnorm(n) + z
idx = 1:n
d = list(y=y,idx=idx,z=z)
eig.max <- max(eigen(Cm)$values)
formula = y ~ f(idx, model = "generic1", Cmatrix = Cm,
                max.eigenvalue = eig.max) + z
result = inla(formula, data=d,family="gaussian",
              control.family = list(initial = log(1/sd^2), fixed=TRUE),
              verbose=T, keep = T)
