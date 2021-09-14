iid = function (cmd = c("graph", "Q", "mu", "initial", "log.norm.const", 
    "log.prior", "quit"), theta = NULL) 
{
    envir = parent.env(environment())

    interpret.theta = function() {
        return(list(prec = exp(theta[1L])))
    }
    graph = function() {
        G = Diagonal(n, x = rep(1, n))
        return(G)
    }
    Q = function() {
        prec = interpret.theta()$prec
        Q = Diagonal(n, x = rep(prec, n))
        return(Q)
    }
    mu = function() {
        return(numeric(0))
    }
    log.norm.const = function() {
        prec = interpret.theta()$prec
        val = sum(dnorm(rep(0, n), sd = 1/sqrt(prec), log = TRUE))
        return(val)
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
    if (is.null(theta)) 
        theta = initial()
    val = do.call(match.arg(cmd), args = list())
    return(val)
}


n = 50
y = rnorm(n)
rgen = inla.rgeneric.define(model = iid, n=n)
r = inla(y ~ -1 + f(idx, model=rgen),
         data = data.frame(y, idx = 1:n),
         verbose = TRUE)

