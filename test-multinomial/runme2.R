ng = 2 ## groups
n = 1000 ## replications
size = 20 ## size

y = matrix(NA, n, ng)
z = matrix(NA, n, ng)
p = matrix(NA, n, ng)
g = matrix(NA, n, ng)

for(i in 1:n) {
    g[i,] = i  ## observation id
    z[i,] = runif(ng)
    p[i,] = exp(z[i,])/sum(exp(z[i,])) 
    y[i,] = rmultinom(n=1, size=size, prob = p[i,])
}

## need vectors, not matrices
y = as.vector(y)
g = as.vector(g)
z = as.vector(z)

## the formulation model="iid",initial=-5,fixed=T is used to model the
## observation spesific intercept
formula = y ~ z + f(g, model="iid",
                    hyper = list(prec = list(initial=-5,fixed=T))) -1

r = inla(formula, data = data.frame(y,z,g), family = "poisson", 
        control.predictor=list(compute=TRUE))
summary(r)

### plug-in estimate of p
a = r$summary.linear.predictor[1:n, "mean"]
b = r$summary.linear.predictor[1:n + n, "mean"]
p.hat = exp(a)/(exp(a) + exp(b))

plot(p[, 1],  p.hat)
points(p[, 2], 1-p.hat)
print(mean(abs(p[,1]-p.hat)))
