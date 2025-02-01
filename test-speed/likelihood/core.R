set.seed(12345)
n <- 10^4
inla.setOption(num.threads = "1:1", safe = FALSE, verbose = FALSE)
cat(sep="", "\n")
debug <- !TRUE

y <- rnorm(n)
r <- inla(y ~ 1, data = data.frame(y), family = "stdnormal")
cat(sep="", "stdnormal ", r$cpu.used[2], "\n")
if (debug) cat("\t", "loglikelihood ", r$mlik[2], "\n")

y <- rnorm(n)
r <- inla(y ~ 1, data = data.frame(y), family = "stdnormal",
          control.expert = list(disable.gaussian.check = TRUE))
cat(sep="", "stdnormal(disable=TRUE) ", r$cpu.used[2], "\n")
if (debug) cat("\t", "loglikelihood ", r$mlik[2], "\n")

t.ref <- Sys.time()

for(link in c("logit", "probit", "cloglog"))
{
    y <- rbinom(n, size = 1, prob = 0.5)
    r <- inla(y ~ 1, data = data.frame(y),
              family = "binomial",
              control.family = list(
                  link = link))
    cat(sep="", "binomial(size=1,", link, ") ", r$cpu.used[2], "\n")

    y <- rbinom(n, size = 10, prob = 0.5)
    r <- inla(y ~ 1, data = data.frame(y, m = 10),
              Ntrials = m,
              family = "binomial", 
              control.family = list(
                  link = link))
    cat(sep="", "binomial(size=10,", link, ") ", r$cpu.used[2], "\n")

    y <- rbinom(n, size = 100, prob = 0.5)
    r <- inla(y ~ 1, data = data.frame(y, m = 100),
              Ntrials = m, family = "binomial", 
              control.family = list(
                  link = link))
    cat(sep="", "binomial(size=100,", link, ") ", r$cpu.used[2], "\n")

    y <- rbinom(n, size = 1, prob = 0.5)
    r <- inla(y ~ 1, data = data.frame(y),
              family = "betabinomial",
              control.family = list(
                  hyper = list(rho = list(fixed = TRUE)), 
                  link = link))
    cat(sep="", "betabinomial(size=1,", link, ") ", r$cpu.used[2], "\n")

    y <- rbinom(n, size = 10, prob = 0.5)
    r <- inla(y ~ 1, data = data.frame(y, m = 10),
              Ntrials = m, family = "betabinomial", 
              control.family = list(
                  hyper = list(rho = list(fixed = TRUE)), 
                  link = link))
    cat(sep="","betabinomial(size=10,", link, ") ", r$cpu.used[2], "\n")

    y <- rbinom(n, size = 100, prob = 0.5)
    r <- inla(y ~ 1, data = data.frame(y, m = 100),
              Ntrials = m, family = "betabinomial", 
              control.family = list(
                  hyper = list(rho = list(fixed = TRUE)), 
                  link = link))
    cat(sep="", "betabinomial(size=100,", link, ") ", r$cpu.used[2], "\n")
}

y <- rpois(n, 1)
r <- inla(y ~ 1, data = data.frame(y),
          family = "poisson")
cat(sep="", "poisson ", r$cpu.used[2], "\n")

y <- rgamma(n, shape = 1, rate = 1)
r <- inla(y ~ 1, data = data.frame(y),
          family = "gamma",
          control.family = list(
              hyper = list(prec = list(initial = 0,
                                       fixed = TRUE))))
cat(sep="", "gamma ", r$cpu.used[2], "\n")

y <- rgamma(n, shape = 1, rate = 1)
r <- inla(y ~ 1, data = data.frame(y),
          family = "gamma",
          control.family = list(
              hyper = list(prec = list(initial = 0,
                                       fixed = TRUE)),
              control.link = list(
                  model = "quantile",
                  quantile = 0.5)))
cat(sep="", "gamma(quantile) ", r$cpu.used[2], "\n")

###
###
###
cat("\nTOTAL TIME ", Sys.time() - t.ref, "\n")
