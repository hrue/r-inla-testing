inla.fixed.effect.model <- function (cmd = c("graph", "Q", "mu", "initial", "log.norm.const", 
    "log.prior", "quit"), theta = NULL) 
{
    n <- 1
    
    envir <- parent.env(environment())

    graph <- function() {
        G <- Diagonal(n, x = rep(1, n))
        return(G)
    }

    Q <- function() {
        Q <- Diagonal(n, x = rep(prior.prec, n))
        return(Q)
    }

    mu <- function() {
        return(rep(prior.mean, n))
    }

    log.norm.const <- function() {
        val <- sum(dnorm(rep(0, n), sd = 1/sqrt(prior.prec), log = TRUE))
        return(val)
    }

    log.prior <- function() {
        return(numeric(0))
    }

    initial <- function() {
        return(numeric(0))
    }

    quit <- function() {
        return(invisible())
    }

    val <- do.call(match.arg(cmd), args = list())
    return(val)
}


n <- 100
a <- scale(rnorm(n))
mu <- 0.2 * a
y <- rpois(n, lambda = exp(mu))

data <- tibble(y = y, a = a, idx = rep(1, n), r = 1:n)

p <- 10^(seq(-4, 9, by = 1))
aa.mode <- c()
aa.mean <- c()
aa.sd <- c()
for(prec_a in p) {
    fixed.effect <- inla.rgeneric.define(inla.fixed.effect.model, prior.mean = 1, prior.prec = prec_a)
    fit2 <- inla(y ~ -1 + f(idx, a, model = fixed.effect), data = data, family = "poisson", 
                 control.fixed = list(mean = list(a = 0.2), prec = list(a = prec_a)),
                 control.inla = list(int.strategy = "eb", control.vb = list(enable = !FALSE,
                                                                            emergency = 2500)),
                 verbose = T)
    print(fit2$summary.random$idx[, c("mean", "sd")])
    aa.mean <- c(aa.mean, fit2$summary.random$idx[, "mean"])
    aa.mode <- c(aa.mode, fit2$mode$x[n+1])
    aa.sd <- c(aa.sd, fit2$summary.random$idx[, "sd"])
}

par(mfrow = c(2, 2))
plot(p, aa.mean, log = "x", main = "mean")
plot(p, aa.mode, log = "x", main = "mode")
plot(p, aa.sd, log = "xy", main = "sd")
