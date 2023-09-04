library(evd)
map.tail = function(x, interval, inverse = FALSE) {
    if (!inverse) {
        return (interval[1] + (interval[2] - interval[1]) * exp(x)/(1.0 + exp(x)))
    } else {
        return (log((x-interval[1])/(interval[2]-x)))
    }
}
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

n = 300
x = rnorm(n, sd=0.5) # we generate values for x from a N(0,0.5^2) dist.
eta.x = 1 + 0.4*x
spread = 0.3
tail = 0.2
p.alpha = 0.9
p.beta = 0.25
par = giveme.gev.par(q = eta.x, sbeta = spread, alpha = p.alpha, beta = p.beta,
                     xi = tail)
y = numeric(n)
for(i in 1:n) {
    y[i] = rgev(1, loc = par$mu[i], scale = par$sigma, shape = par$xi)
}

hyper.spread = list(initial = 1,
                    fixed=FALSE,
                    prior = "loggamma",
                    param = c(3, 3))

tail.interval = c(0, 0.5)
tail.intern = map.tail(tail, tail.interval, inverse=TRUE)
hyper.tail = list(initial = if (tail == 0.0) -Inf else tail.intern,
                  prior = "pc.gevtail",
                  param = c(5, tail.interval),
                  fixed= if (tail == 0.0) TRUE else FALSE)
hyper.bgev = list(spread = hyper.spread,
                  tail = hyper.tail)                  
control.bgev = list(q.location = p.alpha,
                    q.spread = p.beta,
                    ## quantile levels for the mixing part
                    q.mix= c(0.05, 0.20),
                    ## the Beta(s1, s2) mixing distribution parameters.
                    ## Hard-coded for the moment: s1=s2=5
                    beta.ab = 5)

data.bgev = data.frame(y = y, intercept = 1, x = x)
formula = inla.mdata(y, matrix(ncol = 0, nrow = n), matrix(ncol = 0, nrow = n)) ~ -1 + intercept + x
r = inla(formula, 
         data = data.bgev, 
         family = "bgev",
         control.family = list(hyper = hyper.bgev,
                               control.bgev = control.bgev),
         control.predictor = list(compute = TRUE),
         control.fixed = list(prec.intercept = 1, prec=1),
         control.inla = list(cmin = 0, int.strategy = "eb", b.strategy = "keep"),
         verbose=!FALSE, 
         inla.call = "inla.mkl.work")
rr = inla(formula, 
         data = data.bgev, 
         family = "bgev",
         control.family = list(hyper = hyper.bgev,
                               control.bgev = control.bgev),
         control.predictor = list(compute = TRUE),
         control.fixed = list(prec.intercept = 1, prec=1),
         control.inla = list(cmin = Inf, int.strategy = "eb", b.strategy = "keep"),
         verbose=!FALSE, 
         inla.call = "inla.mkl.work")
