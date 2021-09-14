generic = function(cmd = c("graph", "Q", "mu", "initial", "log.norm.const", "log.prior", "quit"),
                   theta = NULL) 
{
    envir = parent.env(environment())

    interpret.theta = function() {
        return(list(prec = exp(theta[1L])))
    }

    graph = function() {
        return(Q())
    }

    Q = function() {
        return (interpret.theta()$prec * Cmatrix)
    }

    mu = function() {
        return(numeric(0))
    }

    log.norm.const = function() {
        prec = interpret.theta()$prec
        return (dim(Cmatrix)[1]/2 * log(prec))
    }

    log.prior = function() {
        prec = interpret.theta()$prec
        val = dgamma(prec, shape = 1, rate = 1, log = TRUE) + 
            theta[1L]
        return(val)
    }

    initial = function() {
        ntheta = 1
        return(rep(1, ntheta))
    }

    quit = function() {
        return(invisible())
    }

    if (length(theta) == 0) {
        theta = initial()
    }
    val = do.call(match.arg(cmd), args = list())
    return(val)
}


library(mvtnorm)
n = 20
S = matrix(rnorm(n^2), n, n)
S = S %*% t(S)
Cmatrix = solve(S)
Cmatrix = (Cmatrix + t(Cmatrix))/2
y = c(rmvnorm(1,  sigma = S))
idx = 1:n
rgen = inla.rgeneric.define(model = generic, Cmatrix = Cmatrix)
r = inla(y ~ -1 + f(idx, model=rgen),
         data = data.frame(y, idx), 
         verbose = TRUE, 
         control.family = list(hyper = list(prec = list(initial = 5,  fixed=TRUE))))
