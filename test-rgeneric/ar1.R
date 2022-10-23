ar1.mean <- function (cmd = c("graph", "Q", "mu", "initial", "log.norm.const", 
    "log.prior", "quit"), theta = NULL) 
{
    envir <- parent.env(environment())

    interpret.theta <- function() {
        return(list(prec = exp(theta[1L]), rho = 2 * exp(theta[2L])/(1 + exp(theta[2L])) - 1))
    }

    graph <- function() {
        i <- c(1L, n, 2L:(n - 1L), 1L:(n - 1L))
        j <- c(1L, n, 2L:(n - 1L), 2L:n)
        G <- inla.as.sparse(sparseMatrix(i = i, j = j, x = 1, 
                                         repr = "T"))
        return(G)
    }

    Q <- function() {
        param <- interpret.theta()
        a <- 1 + param$rho^2
        b <- -param$rho
        x <- param$prec/(1 - param$rho^2) * c(1, b, rep(c(a, b), n - 2), 1)
        return(x)
    }

    mu <- function() {
        return(rep(0, n))
        return(numeric(0))
    }

    log.norm.const <- function() {
        param <- interpret.theta()
        prec.innovation <- param$prec/(1 - param$rho^2)
        val <- n * (-0.5 * log(2 * pi) + 0.5 * log(prec.innovation)) + 
            0.5 * log(1 - param$rho^2)
        return(val)
    }

    log.prior <- function() {
        param <- interpret.theta()
        val <- (dgamma(param$prec, shape = 1, rate = 1, log = TRUE) + 
                theta[1L] + dnorm(theta[2L], mean = 0, sd = 1, log = TRUE))
        return(val)
    }

    initial <- function() {
        return(rep(1, 2))
    }

    quit <- function() {
        return(invisible())
    }

    if (!length(theta)) {
        theta <- initial()
    }

    val <- do.call(match.arg(cmd), args = list())

    return(val)
}

n = 10000
rho=0.9
y = arima.sim(n, model = list(ar = rho)) * sqrt(1-rho^2)
idx = 1:n
model = inla.rgeneric.define(ar1.mean, n=n, ntheta = 2, debug=FALSE, optimize = TRUE)
formula = y ~ -1 + f(idx, model="rgeneric", rgeneric = model, n = n)
inla.setOption(inla.mode = "experimental")
r = inla(formula, data = data.frame(y, idx), family = "gaussian", num.threads = 1, verbose = T)
