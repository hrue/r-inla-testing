`rgeneric.test` = function(
    cmd = c("graph", "Q", "mu", "initial", "log.norm.const", "log.prior", "quit"),
    theta = NULL)
{
    envir = parent.env(environment())

    graph = function() {
        return(matrix(1, n, n))
    }

    Q = function() {
        R <- matrix(sin(1:n^2), n, n)
        R <- R %*% t(R)
        diag(R) <- diag(R)+1
        Q <- exp(theta[1]) * R
        return(Q)
    }

    mu = function() return (numeric(0))

    log.norm.const = function() {
        return (numeric(0))
    }

    log.prior = function() {
        return (dgamma(exp(theta[1]), shape = 1, rate = 1, log=TRUE) + theta[1])
    }

    initial = function() {
        return(4)
    }

    if (!length(theta)) theta = initial()
    val = do.call(match.arg(cmd), args = list())

    return (val)
}

`rgeneric.test.opt.1` = function(
    cmd = c("graph", "Q", "mu", "initial", "log.norm.const", "log.prior", "quit"),
    theta = NULL)
{
    envir = parent.env(environment())

    if (!exists("cache.done", envir = envir)) {
        R <- matrix(sin(1:n^2), n, n)
        R <- R %*% t(R)
        diag(R) <- diag(R)+1
        R.logdet <- log(det(R))
        R <- inla.as.sparse(R)
        idx <- which(R@i <= R@j)
        R@i <- R@i[idx]
        R@j <- R@j[idx]
        R@x <- R@x[idx]
        assign("R", R, envir = envir)
        norm.const <- -n/2 * log(2*pi) + 0.5 * R.logdet
        assign("norm.const", norm.const, envir = envir)
        assign("cache.done", TRUE, envir = envir)
    }

    graph = function() {
        return (R)
    }

    Q = function() {
        return(exp(theta[1]) * R)
    }

    mu = function() return (numeric(0))

    log.norm.const = function() {
        return (norm.const + n/2 * theta[1])
    }

    log.prior = function() {
        return (dgamma(exp(theta[1]), shape = 1, rate = 1, log=TRUE) + theta[1])
    }

    initial = function() {
        return(4)
    }

    if (!length(theta)) theta = initial()
    val = do.call(match.arg(cmd), args = list())

    return (val)
}

`rgeneric.test.opt.2` = function(
    cmd = c("graph", "Q", "mu", "initial", "log.norm.const", "log.prior", "quit"),
    theta = NULL)
{
    envir = parent.env(environment())

    if (!exists("cache.done", envir = envir)) {
        R <- matrix(sin(1:n^2), n, n)
        R <- R %*% t(R)
        diag(R) <- diag(R)+1
        R.logdet <- log(det(R))
        R <- inla.as.sparse(R)
        idx <- which(R@i <= R@j)
        R@i <- R@i[idx]
        R@j <- R@j[idx]
        R@x <- R@x[idx]
        assign("R", R, envir = envir)
        norm.const <- -n/2 * log(2*pi) + 0.5 * R.logdet
        assign("norm.const", norm.const, envir = envir)
        assign("cache.done", TRUE, envir = envir)
    }

    graph = function() {
        return (R)
    }

    Q = function() {
        ## since R was created with 'inla.sparse.matrix' above, the indices are sorted in a
        ## spesific order. This ordering is REQUIRED for R@x to be interpreted correctly.
        return(exp(theta[1]) * R@x)
    }

    mu = function() return (numeric(0))

    log.norm.const = function() {
        return (norm.const + n/2 * theta[1])
    }

    log.prior = function() {
        return (dgamma(exp(theta[1]), shape = 1, rate = 1, log=TRUE) + theta[1])
    }

    initial = function() {
        return(4)
    }

    if (!length(theta)) theta = initial()
    val = do.call(match.arg(cmd), args = list())

    return (val)
}

inla.setOption(num.threads = "1:1", inla.call = "inla.mkl.work")
n = 100
s = .1
Q <- rgeneric.test("Q", theta = 0)
library(mvtnorm)
S <- solve(as.matrix(Q))
S <- (S + t(S))/2
x <- drop(rmvnorm(1, sigma = S))
y <- x + rnorm(n, sd = s)
cont.family = list(hyper = list(prec = list(initial=log(1/s^2), fixed=TRUE)))

r1 = inla(y ~ -1 + f(idx, model="generic", Cmatrix = Q,
                     hyper = list(prec = list(prior = "loggamma", param = c(1, 1)))), 
          data = data.frame(y = y, idx = 1:n), control.family = cont.family)
ld <- 0.5 * log(det(as.matrix(Q)))
r1$mlik <- r1$mlik + ld

model2 = inla.rgeneric.define(rgeneric.test, n=n, optimize = FALSE)
r2 = inla(y ~ -1 + f(idx, model=model2), 
          data = data.frame(y = y, idx = 1:n), control.family = cont.family)

model3 = inla.rgeneric.define(rgeneric.test.opt.1, n=n, optimize = FALSE)
r3 = inla(y ~ -1 + f(idx, model=model3), 
          data = data.frame(y = y, idx = 1:n), control.family = cont.family)

model4 = inla.rgeneric.define(rgeneric.test.opt.2, n=n, optimize = TRUE)
r4 = inla(y ~ -1 + f(idx, model=model4), 
          data = data.frame(y = y, idx = 1:n), control.family = cont.family)

print(r2$mlik - r1$mlik)
print(r3$mlik - r1$mlik)
print(r4$mlik - r1$mlik)

print(rbind(native = r1$cpu[2], 
            rgeneric.plain = r2$cpu[2], 
            rgeneric.cache = r3$cpu[2], 
            rgeneric.optimze = r4$cpu[2]))
