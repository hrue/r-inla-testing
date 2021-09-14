iid.model <- function(
    cmd = c("graph", "Q", "mu", "initial", "log.norm.const", "log.prior", "quit"),
    theta = NULL)
{
    ## this is an example of the 'rgeneric' model. here we implement the iid model as described
    ## in inla.doc("iid"), without the scaling-option

    ## variables defined the in the define-call, are stored here
    ## (which is in the path)
    envir = parent.env(environment())
    
    interpret.theta = function()
    {
        return (list(prec = exp(theta[1L])))
    }

    graph = function()
    {
        G = Diagonal(n, x= rep(1, n))
        return (G)
    }

    Q = function()
    {
        if (exists("init.cache", envir = envir)) {
            INLA:::inla.my.update()
            assign("init.cache", TRUE, envir = envir)
        }

        prec = interpret.theta()$prec
        Q = Diagonal(n, x= rep(prec, n))
        return (Q@x)
    }

    mu = function()
    {
        return (numeric(0))
    }
    
    log.norm.const = function()
    {
        return (numeric(0))
    }

    log.prior = function()
    {
        prec = interpret.theta()$prec
        val = dgamma(prec, shape = 1, rate = 1, log=TRUE) + theta[1L]
        return (val)
    }

    initial = function()
    {
        ntheta = 1
        return (rep(1, ntheta))
    }

    quit = function()
    {
        return (invisible())
    }

    ## if theta is not required, it is not set. we set it here, for convenience.
    if (!length(theta))
        theta = initial()
    
    val = do.call(match.arg(cmd), args = list())
    return (val)
}


n = 100
nr <- 1
ng <- 1
s = 0.1
x = rnorm(n)
y = 1 + x + rnorm(n, sd = s)

rr <- sample(1:nr, n, replace = TRUE)
gg <- sample(1:ng, n, replace = TRUE)
model = inla.rgeneric.define(iid.model, n=n, debug=TRUE, optimize = TRUE)
r2 = (inla(y ~ -1 + f(idx, model=model, replicate = rr, group = gg), 
           data = data.frame(y = y, idx = 1:n, rr = rr, gg = gg),
           control.family = list(
               hyper = list(prec = list(initial = 10, fixed=TRUE)))))

r1 = (inla(y ~ -1 + f(idx, model="iid", param = c(1, 1), replicate = rr, group = rr), 
           data = data.frame(y = y, idx = 1:n, rr = rr, gg = gg),
           control.family = list(
               hyper = list(prec = list(initial=10, fixed=TRUE)))))

print(r1$mlik - r2$mlik)

