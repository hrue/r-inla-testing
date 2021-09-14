test.model = function (cmd = c("graph", "Q", "mu", "initial", "log.norm.const", 
    "log.prior", "quit"), theta = NULL) 
{
    envir = environment(sys.call()[[1]])

    if (!is.null(envir) && !exists("first.time", envir=envir)) {
        cat("\n\ndo first time work...\n")
        assign("first.time", TRUE, envir = envir)
        dyn.load("prior.so")
        cat("do a test call\n")
        value = 1.0
        th = 2.0
        out = .C("prior", value = as.double(value), theta = as.double(th))
        cat("prior gives ",  out$value, "\n\n\n\n")
    }

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
        value = 1.0
        out = .C("prior", value = as.double(value), theta = as.double(theta))
        cat("prior gives ",  out$value, "\n")
        return(out$value)
    }
    initial = function() {
        ntheta = 1
        return(rep(1, ntheta))
    }
    quit = function() {
        return(invisible())
    }
    if (length(theta) == 0)
        theta = initial()
    val = do.call(match.arg(cmd), args = list())
    return(val)
}

iid = inla.rgeneric.define(test.model, n=10)

r = inla(y ~ -1 + f(idx, model=iid),
         data = data.frame(y=1:10, idx = 1:10),
         verbose=TRUE)

