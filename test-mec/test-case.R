n = 10L
beta.x = 4
prec.x = 1
prec.y = 1

if (n == 1L) {
    X = 1
    W = 1
    Y = beta.x * X
} else {
    X <- rnorm(n)
    W <- X + rnorm(n, sd = sqrt(1/prec.x))
    Y <- beta.x*X + rnorm(n, sd = sqrt(1/prec.y))
}

log.post.beta = function(beta, y, prec.y, w, prec.w)
{
    nbeta = length(beta)
    dbeta = numeric(nbeta)
    n = length(y)
    for(i in 1:nbeta) {
        bbeta = beta[i]
        cc = bbeta^2 * prec.y + prec.w
        cond.mean = (bbeta * prec.y * y + prec.w * w) / cc
        dbeta[i] = -0.5 * prec.y * sum((y- bbeta * cond.mean)^2) -
            0.5 * prec.x * sum((w - cond.mean)^2) - n/2 * log(cc)
    }
    return (dbeta - max(dbeta))
}

beta = seq(max(0.001, beta.x-4),  beta.x+4,  length=1000)
dbeta = exp(log.post.beta(beta, Y, prec.y, W, prec.x))
dbeta = dbeta / sum(dbeta) / diff(beta)[1]
plot(beta,  dbeta, type="l", lwd=2)

prec.beta= 0.0000000001

## prior parameters for gamma
a <-1
b <-5e-04

formula = my.y ~ -1 + f(me.fixed.effect, model="iid",
	hyper = list(prec = list(initial = -20, fixed=TRUE)))
me.fixed.effect = rep(1:n, 2)
my.y = matrix(NA, 2*n, 2)
my.y[1:n, 1] = Y
my.y[(1:n) + n, 2] = W

r = inla(formula,
	data = list(my.y=my.y, me.fixed.effect = me.fixed.effect),
        control.predictor = list(initial = 15, compute=TRUE), 
	family = c("gaussian", "mefixedeffect"),
	control.data = list(
		list( 
                     hyper = list(
                             prec = list(
                                     initial = log(prec.y),
                                     param=c(a, b),
                                     fixed=TRUE)
                             )
                     ),
		list(
                     hyper = list(
                             beta = list(
                                     initial = beta.x,
                                     prior="normal",
                                     param=c(beta.x, prec.beta),
                                     fixed=FALSE
                                     ),
                             prec = list(
                                     initial=log(prec.x),
                                     param = c(a, b), 
                                     fixed=TRUE)
                             )
                     )
                ),
        ##control.inla = list(tolerance = 1e-6, h=1e-5, verbose=FALSE), 
	verbose=TRUE)

rr = inla.hyperpar(r, diff.logdens=10, dz=0.1)

m = rr$internal.marginals.hyperpar[[1]]
func = splinefun(m[, 1], log(m[, 2]))
bb = unique(pmin(max(m[, 1]), pmax(min(m[, 1]), beta)))
ddbeta = exp(func(bb))
ddbeta = ddbeta / sum(ddbeta) / mean(diff(bb))
lines(bb, ddbeta, col="red", lwd=3)

## mcmc

gmrf.sample = function(Qdiag, b)
{
    ## sample a GMRF with Q = diag(Qdiag) and (canonical) parameter b.
    n = length(Qdiag)
    mu = b/Qdiag
    z = rnorm(n)
    x = z/sqrt(Qdiag)
    return (x + mu)
}

xx = X
bbeta = beta.x
n.iter = 10000L
beta.trace = numeric(n.iter)
for(iter in 1:n.iter) {

    Qdiag = (prec.y * bbeta^2  + prec.x) * rep(1, n)
    b = prec.x * W + bbeta * prec.y * Y
    xx = gmrf.sample(Qdiag, b)

    Qdiag = prec.y * sum(xx^2)
    b = prec.y * sum(Y * xx)
    bbeta = gmrf.sample(Qdiag, b)
    beta.trace[iter] = bbeta
}

hist(beta.trace, n = 100L, prob=TRUE, add=TRUE)
