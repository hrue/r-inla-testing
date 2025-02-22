library(INLA)

rpom = function(alpha, eta) 
{
    ## alpha: the cutpoints. eta: the linear predictor
    F = function(a, eta) 1.0/(1+exp(-(a-eta)))

    ns = length(eta)
    y = numeric(ns)
    nc = length(alpha) + 1

    for(k in 1:ns) {
        p = diff(c(0.0, F(alpha, eta[k]), 1.0))
        y[k] = sample(1:nc, 1, prob = p)
    }
    return (y)
}

set.seed(123)
n = 300
nsim = 1E5
x = rnorm(n, sd = 1)

eta = x
alpha = c(-1, 0, 0.75)
y = rpom(alpha, eta)
xx = inla.group(x)

## which index to predict
idx.pred <- n
y[idx.pred] <- NA

if (TRUE) {
    r = inla(y ~ -1 +
                 f(xx, model="rw2", scale.model=TRUE, 
                   hyper = list(prec = list(prior = "pc.prec",
                                            param = c(0.1, 0.01)))), 
         data = data.frame(y, x, idx = 1:n, xx),
         family = "pom",
         control.family = list(control.pom = list(cdf = "probit", fast = FALSE)), 
         control.inla = list(cmin = 0), 
         control.fixed = list(prec = 1, prec.intercept = 1), 
         control.compute = list(config = TRUE))
} else {
    r = inla(y ~ -1 + x, 
             data = data.frame(y, x, idx = 1:n),
             family = "pom",
             control.compute = list(config = TRUE), 
             control.inla = list(cmin = 0))
}

if (FALSE) {
    ## this is how to estimate the cutpoints
    theta = inla.hyperpar.sample(nsim, r, intern=TRUE)
    nms = paste(paste0("theta", 1:length(alpha)), "for POM")
    sim.alpha = matrix(NA, dim(theta)[1], length(alpha))
    for(k in 1:length(alpha)) {
        if (k == 1) {
            sim.alpha[, k] = theta[, nms[1]]
        } else {
            sim.alpha[, k] = sim.alpha[, k-1] + exp(theta[, nms[k]])
        }
    }
}


idx.alpha <- grep("theta[1-9] for POM", rownames(r$summary.hyperpar))
K <- length(idx.alpha)
stopifnot(K == max(idx.alpha))

F <- function(x) pnorm(x)
##F <- function(x) inla.link.invlogit

##pred.fun <- function(idx.alpha = NA, idx.pred = NA, F = NA) {
pred.fun <- function() {
    na <- length(idx.alpha)
    stopifnot(na >= 2)
    alpha <- numeric(na)
    gamma <- numeric(na)
    probs <- numeric(na+1)

    alpha[1] <- theta[idx.alpha[1]]
    for(i in 2:na) {
        alpha[i] <- alpha[i-1] + exp(theta[idx.alpha[i]])
    }

    for(i in 1:na) {
        gamma[i] <- alpha[i] - Predictor[idx.pred]
    }

    probs[1] <- F(gamma[1])
    for(i in 2:na) {
        probs[i] <- F(gamma[i]) - F(gamma[i-1])
    }
    probs[na+1] <- 1 - F(gamma[na])
    return (probs)
}

xx <- inla.posterior.sample(10^4, r, intern = TRUE)
pp <- inla.posterior.sample.eval(pred.fun, xx)

par(mfrow = c(2, 2))
for(i in 1:(K+1)) {
    hist(pp[i, ], prob = TRUE, n = 300, main = paste("prob class", i))
}
