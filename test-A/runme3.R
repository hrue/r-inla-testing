##  Generate artificial truth x, observed covariate zz and observed data y
m=100
n=10
A = n-by-m-sparse
mu = 1:m
x = rnorm(m)
z = A %*% mu
y = A %*% x + z + rnorm(n)

## Now we should pretend to only know the observed y and z
## How to complete the model?  z is only as long as the data, n, but
we have m unknowns

node=1:m ## indixes into the unknown spatial component "x"
formula = y ~ z + spde(node,???...???)
r = inla(formula, data = data.frame(y,z,node),
        control.predictor = list(compute=T, A = A))
