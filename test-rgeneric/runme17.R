iid.model <- function (cmd = c("graph", "Q", "mu", "initial", "log.norm.const", 
                               "log.prior", "quit"), theta = NULL) 
{
    ## this parameterisation is for stdev in the interval [LOW, HIGH]
    envir <- parent.env(environment())
    interpret.theta <- function() {
        stdev <- LOW + (HIGH - LOW) * 1/(1+exp(-theta))
        return(list(prec = 1/stdev^2))
    }
    graph <- function() {
        G <- Diagonal(n, x = rep(1, n))
        return(G)
    }
    Q <- function() {
        prec <- interpret.theta()$prec
        Q <- Diagonal(n, x = rep(prec, n))
        return(Q)
    }
    mu <- function() {
        return(numeric(0))
    }
    log.norm.const <- function() {
        prec <- interpret.theta()$prec
        val <- sum(dnorm(rep(0, n), sd = 1/sqrt(prec), log = TRUE))
        return(val)
    }
    log.prior <- function() {
        ## this is for an exponential prior for STDEV with rate RATE
        prec <- interpret.theta()$prec
        stdev <- sqrt(1/prec)
        val <- (dexp(stdev, rate = RATE, log = TRUE) 
            ## change of normalizing constant due to [LOW, HIGH]
            - log(pexp(HIGH, rate = RATE) - pexp(LOW, rate = RATE)) 
            ## log(jacobian)
            + log((HIGH-LOW) * exp(-theta)/(1+exp(-theta))^2))
        return(val)
    }
    initial <- function() {
        return (4)
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


n <- 300
y <- rnorm(n, sd = 0.1) + rnorm(n, sd = 1) ## use stdnormal as family

LOW <- 0
HIGH <- 1
RATE <- 1
riid <- inla.rgeneric.define(iid.model, n = n, LOW = LOW, HIGH = HIGH, RATE = RATE)

r <- inla(y ~ -1 + f(idx, model = riid),
          data = data.frame(y, idx = 1:n),
          family = "stdnormal")
summary(r)

## from theta to stdev
map.fun <- function(theta) LOW + (HIGH-LOW)/(1+exp(-theta))
hist(map.fun(inla.rmarginal(10000, r$marginals.hyperpar[[1]])), n = 100, prob = TRUE)

