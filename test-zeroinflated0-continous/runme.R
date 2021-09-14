## this is an example of Sara's trick for zero-inflated continouse
## resposes.

n = 10000

## model for prob(y=0)
ilogit = function(x) exp(x)/(1+exp(x))
x.1 = rnorm(n, sd = 0.1)
eta.1 = 0 + x.1
p = ilogit(eta.1)
zero.inflated = rbinom(n, prob = p,  size = 1)

## model for y>0
x.2 = rnorm(n,  sd = 0.2)
eta.2 = 2 + 2*x.2
mu = exp(eta.2)
a = 1.2
b = mu / a
y = rgamma(n,  shape = a,  scale = b)

## generate the simulated dataset
yy = numeric(n)
idx = (zero.inflated != 1)
yy[idx] = y[idx]
idx = (zero.inflated == 1)
yy[idx] = 0


## build the model, first the zero inflated one
y.zero = zero.inflated
x.zero = x.1
intercept.zero = rep(1, n)

## then the non-zero ones
idx = which(yy > 0)
nnz = length(idx)
stopifnot(nnz > 0)
y.nonzero = yy[idx]
x.nonzero = x.2[idx]
intercept.nonzero = rep(1, nnz)

##
formula = Y ~ -1 + intercept1 + intercept2 + X1 + X2

Y = matrix(NA, n + nnz,  2)
Y[1:n, 1] = y.zero
Y[n + 1:nnz, 2] = y.nonzero
X1 = c(x.zero, rep(NA, nnz))
X2 = c(rep(NA, n), x.nonzero)
intercept1 = c(intercept.zero, rep(NA, nnz))
intercept2 = c(rep(NA, n), intercept.nonzero)

r = inla(formula, data = list(
                          Y=Y,
                          X1=X1,
                          X2=X2,
                          intercept1 = intercept1,
                          intercept2 = intercept2),
        family = list("binomial",  "gamma"),
        Ntrials = 1,
        verbose = TRUE)
