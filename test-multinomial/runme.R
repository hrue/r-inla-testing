ng = 3 ## groups
n = 1000 ## replications
size = 20 ## size

y = matrix(NA, n, ng)
z = matrix(NA, n, ng)
p = matrix(NA, n, ng)
g = matrix(NA, n, ng)

for(i in 1:n) {
    g[i,] = i  ## observation id
    z[i,] = sample(0:1, size=ng, replace=T) ## covariate
    p[i,] = exp(z[i,])/sum(exp(z[i,])) 
    y[i,] = rmultinom(n=1,size=size, prob = p[i,])
}

## need vectors, not matrices
y = as.vector(y)
g = as.vector(g)
z = as.vector(z)

## the formulation model="iid",initial=-5,fixed=T is used to model the
## observation spesific intercept
formula = y ~ z + f(g, model="iid", hyper = list(prec = list(initial=-5,fixed=T))) -1

r = inla(formula, data = data.frame(y,z,g), family = "poisson", verbose=FALSE)
summary(r)
