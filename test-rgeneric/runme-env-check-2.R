rg.model = function (cmd = c("graph", "Q", "mu", "initial", "log.norm.const", 
    "log.prior", "quit"), theta = NULL) 
{
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
        print(paste("rg: prior, n=", n, "shape, rate=", shape, rate))
        prec = interpret.theta()$prec
        val = dgamma(prec, shape, rate, log = TRUE) + 
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

    val = do.call(match.arg(cmd), args = list())
    return(val)
}

n = 20
idx1 = rep(1:(n/2), each=2)
idx2 = 1:n
y = rnorm(n)

m1 = inla.rgeneric.define(rg.model, n=n/2, shape=1, rate=2)
m2 = inla.rgeneric.define(rg.model, n=n, shape=10, rate=20)

r = inla(y ~ -1 + f(idx1, model=m1) + f(idx2, model=m2),
         data = data.frame(y, idx1, idx2),
         verbose=TRUE)
