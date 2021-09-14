`test.rgeneric` = function(
    cmd = c("graph", "Q", "mu", "initial", "log.norm.const", "log.prior", "quit"),
    theta = NULL)
{
    envir = parent.env(environment())
    if (!exists("init.cache", envir = envir)) {
        ## initialize the cache
        A <- matrix(1, 10, 10)
        B <- rep(list(list()), 10)
        assign("A", A, envir = envir)
        assign("B", B, envir = envir)
        assign("init.cache", TRUE, envir = envir)
        print(ls(envir = envir))
    }

    stopifnot(exists("init.cache", envir = envir))
    stopifnot(exists("A", envir = envir))
    stopifnot(exists("B", envir = envir))


    ## this is an example of the 'rgeneric' model. here we implement
    ## the AR-1 model as described in inla.doc("ar1"), where 'rho' is
    ## the lag-1 correlation and 'prec' is the *marginal* (not
    ## conditional) precision.
    
    interpret.theta = function()
    {
        ## internal helper-function to map the parameters from the internal-scale to the
        ## user-scale
        return (list(prec = exp(theta[1L]),
                     rho = 2*exp(theta[2L])/(1+exp(theta[2L])) - 1.0,
                     beta = theta[3L]))
    }

    graph = function()
    {
        ## return the graph of the model. the values of Q is only interpreted as zero or
        ## non-zero. return a sparse.matrix
        if (FALSE) {
            ## slow and easy: dense-matrices
            G = toeplitz(c(1, 1, rep(0, n-2L)))
            G = inla.as.sparse(G)
        } else {
            ## faster. we only need to define the lower-triangular of G
            i = c(
                ## diagonal
                1L, n, 2L:(n-1L),
                ## off-diagonal
                1L:(n-1L))
            j = c(
                ## diagonal
                1L, n, 2L:(n-1L),
                ## off-diagonal
                2L:n)
            x = 1 ## meaning that all are 1
            G = sparseMatrix(i=i, j=j, x=x, giveCsparse = FALSE)
        }            
        return (G)
    }

    Q = function() {
        ## returns the precision matrix for given parameters
        param = interpret.theta()
        if (FALSE) {
            ## slow and easy: dense-matrices
            Q = param$prec/(1-param$rho^2) * toeplitz(c(1+param$rho^2, -param$rho, rep(0, n-2L)))
            Q[1, 1] = Q[n, n] = param$prec/(1-param$rho^2)
            Q = inla.as.sparse(Q)
        } else {
            ## faster. we only need to define the lower-triangular Q!
            i = c(
                ## diagonal
                1L, n, 2L:(n-1L),
                ## off-diagonal
                1L:(n-1L))
            j = c(
                ## diagonal
                1L, n, 2L:(n-1L),
                ## off-diagonal
                2L:n)
            x = param$prec/(1-param$rho^2) *
                c(  ## diagonal
                    1L, 1L, rep(1+param$rho^2, n-2L),
                    ## off-diagonal
                    rep(-param$rho, n-1L))
            Q = sparseMatrix(i=i, j=j, x=x, giveCsparse=FALSE)
        }            
        return (Q)
    }

    mu = function()
    {
        return (1:n * theta[3])
    }
        
    log.norm.const = function()
    {
        ## return(numeric(0))
        ## return the log(normalising constant) for the model
        param = interpret.theta()
        prec.innovation  = param$prec / (1.0 - param$rho^2)
        val = n * (- 0.5 * log(2*pi) + 0.5 * log(prec.innovation)) + 0.5 * log(1.0 - param$rho^2)
        return (val)
    }

    log.prior = function()
    {
        ## return the log-prior for the hyperparameters. the '+theta[1L]' is the log(Jacobian)
        ## for having a gamma prior on the precision and convert it into the prior for the
        ## log(precision).
        param = interpret.theta()
        val = (dgamma(param$prec, shape = 1, rate = 1, log=TRUE) + theta[1L] + 
                   dnorm(theta[2L], mean = 0, sd = 1, log=TRUE) +
                       dnorm(theta[3L], mean = 0, sd = 1, log=TRUE))
               
        return (val)
    }

    initial = function()
    {
        return (c(4, 1, 0.1))
    }

    quit = function()
    {
        return (invisible())
    }

    cmd = match.arg(cmd)
    val = do.call(cmd, args = list())
    return (val)
}


n = 100
rho=0.9
mu = 0.1 * (1:n)
s = 0.1
if (!exists("XX")) {
    XX = mu + arima.sim(n, model = list(ar = rho)) * sqrt(1-rho^2)
    YY = 1 + XX + rnorm(n, sd = s)
}
idx = 1:n
model = inla.rgeneric.define(test.rgeneric, n=n)
formula = YY ~ 1 + f(idx, model= model)
r = inla(formula, data = data.frame(YY, idx), family = "gaussian",
    control.family = list(hyper = list(prec = list(initial = log(1/s^2), fixed=TRUE))),
    control.inla = list(tolerance = 1e9, h=1e-4), 
    control.compute = list(openmp.strategy = "pardiso.parallel"), 
    ##inla.call = "remote", 
    verbose=T, keep=T)
