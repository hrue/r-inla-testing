require(INLA)
library(evd)

giveme.gev.par = function(q, sbeta, alpha, beta, xi) 
{
    .mu = function(q, sbeta, alpha, beta, xi) {
        a = -log(1-beta/2)
        b = -log(beta/2)
        c = -log(alpha)
        if (all(xi > 0.0)) {
            tmp0 = (c^(-xi) - 1)/xi
            tmp1 = a^(-xi)
            tmp2 = b^(-xi)
            dbeta = (tmp1 - tmp2)/xi
            return(q - (sbeta/dbeta) * tmp0)
        } else if (all(xi == 0.0)) {
            dbeta = log(b) - log(a)
            tmp0 = log(c)
            return(q + (sbeta/dbeta) * tmp0)
        } else {
            stop("mixed case not implemented")
        }
    }

    .sigma = function(q, sbeta, alpha, beta, xi) {
        a = -log(1-beta/2)
        b = -log(beta/2)
        if (all(xi > 0.0)) {
            tmp1 = a^(-xi)
            tmp2 = b^(-xi)
            dbeta = (tmp1 - tmp2)/xi
            return(sbeta/dbeta)
        } else if (all(xi == 0.0)) {
            dbeta = log(b) - log(a)
            return(sbeta/dbeta)
        } else {
            stop("mixed case not implemented")
        }
    }

    return(list(mu = .mu(q, sbeta, alpha, beta, xi),
                sigma = .sigma(q, sbeta, alpha, beta, xi),
                xi = xi))
}

map.tail = function(x, interval, inverse = FALSE) {
    if (!inverse) {
        return (interval[1] + (interval[2] - interval[1]) * exp(x)/(1.0 + exp(x)))
    } else {
        return (log((x-interval[1])/(interval[2]-x)))
    }
}

##set.seed(123)
n = 300

## below: set 'spread.x[] = 0',  'tail.x[] = 0' to ignore it

## model for the location/quantile
eta.x = rnorm(n)
eta = 1 + 0.4 * eta.x

## model for log(spread)
spread.x = rnorm(n)
spread.true = 1.0
spread = spread.true * exp(0.3 * spread.x)

## model for the (internal) tail parameter
tail.interval = c(0, 0.5)
tail.x = rnorm(n)
tail.x[] = 0
tail.true = 0.1
tail.true.intern = map.tail(tail.true, tail.interval, inverse=TRUE)
tail = map.tail(tail.true.intern + 0.1 * tail.x, interval = tail.interval)
tail.lambda = 7 ## pc-prior

## the quantile levels for the bgev,  location and spread
p.alpha = 0.5  
p.beta = 0.25  

par = giveme.gev.par(q = eta, sbeta = spread, alpha = p.alpha, beta = p.beta, xi = tail)
y = numeric(n)
for(i in 1:n) {
    y[i] = rgev(1, loc = par$mu[i], scale = par$sigma[i], shape = par$xi[i])
}

## with more covariates you need to use in the formula
## inla.mdata(y, cbind(spread.x, spread.xx), cbind(tail.x, tail.xx)) ~ ...
## as it interpret this as three matrix-blocks, 'y', 'spread' and 'tail'.
## use 'null.matrix' to say there is no covariates
null.matrix = matrix(nrow = n, ncol= 0)
if (all(spread.x == 0)) spread.x = null.matrix
if (all(tail.x == 0)) tail.x = null.matrix

r = inla(
    inla.mdata(y, spread.x, tail.x) ~ 1 + eta.x, 
    family = "bgev",
    data = data.frame(y, eta.x, spread.x, tail.x),
    control.compute = list(cpo = TRUE), 
    control.predictor = list(compute = TRUE), 
    control.family = list(
        dummy = 0,  ## I need to fix why I need to do this...
        control.bgev = list(q.location = p.alpha,
                            q.spread = p.beta,
                            ## quantile levels for the mixing part
                            q.mix= c(0.05, 0.20), 
                            ## the Beta(a, b) mixing distribution.
                            ## hard-coded for the moment
                            beta.ab = 5),
        hyper = list(spread = list(initial = 0,
                                   fixed=FALSE,
                                   initial = 1, 
                                   prior = "loggamma",
                                   param = c(3, 3)), 
                     tail = list(initial = if (tail.true == 0.0) -Inf else tail.true.intern, 
                                 prior = "pc.gevtail",
                                 ## the parameters are (lambda, low, high)
                                 param = c(tail.lambda, tail.interval), 
                                 fixed= if (tail.true == 0.0) TRUE else FALSE), 
                     beta1 = list(prior = "normal",
                                  param = c(0, 300),
                                  initial = 0), 
                     beta2 = list(prior = "normal",
                                  param = c(0, 300),
                                  initial = 0))), 
    control.inla = list(int.strategy = "eb"), 
    control.fixed = list(prec = 10,  prec.intercept = 10), 
    ##num.threads = 8,
    verbose=TRUE)

summary(r)
plot(r, single = TRUE, plot.prior = TRUE)

