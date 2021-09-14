library(mvtnorm)
range = 0.2
n = 50
nr = 20
loc = matrix(runif(2*n), ncol = 2, nrow = n)
var = 1.0
nu = 0.5
S = matrix(0, n, n)
for(i in 1:n) {
for(j in i:n) {
dif = loc[i, ] - loc[j, ]
d = sqrt(sum(dif^2))
S[i, j] = var * INLA:::inla.matern.cf(d, range = range, nu = nu);
S[j, i] = S[i, j]
}
}
y = c(t(rmvnorm(nr, sigma = S)))
r2 = inla(y ~ -1 + f(idx, model="dmatern", locations = loc, replicate = re,
## placing the prior at the correct value, just for
## demonstration
hyper = list(range = list(initial = log(range),
param = c(range, 0.5)))),
data = data.frame(y, idx = rep(1:n, nr), re = rep(1:nr, each = n)),
family = "gaussian",
## just this this at some high value
control.family = list(hyper = list(
prec = list(initial = 12, fixed=TRUE))),
verbose=TRUE)


