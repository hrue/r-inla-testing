trace.output <- FALSE
set.seed(12345)
n <- 2*10^6
inla.setOption(verbose = FALSE, num.threads = "1:12", safe = FALSE, verbose = FALSE)
cat(sep="", "\n")

Sys.setenv(INLA_TRACE = 'GMRFLib_opt_f_intern')

fun <- function(r) {
    a <- r$logfile[grep("opt_f_intern", r$logfile)]
    a <- a[last(grep("Leave,", a))]
    a <- strsplit(a, " ")[[1]]
    a <- a[length(a) -1]
    return (as.numeric(a)/10^6)
}

y <- rnorm(n)
r <- inla(y ~ 1, data = data.frame(y), family = "stdnormal")
cat(sep="", "stdnormal ", fun(r), "\n")

y <- rnorm(n)
r <- inla(y ~ 1, data = data.frame(y), family = "stdnormal",
          control.expert = list(disable.gaussian.check = TRUE))
cat(sep="", "stdnormal(disable=TRUE) ", fun(r), "\n")

y <- rpois(n, 1)
r <- inla(y ~ 1, data = data.frame(y),
          family = "poisson")
##control.numa = list(enable = FALSE))
cat(sep="", "poisson ", fun(r), "\n")

t.ref <- Sys.time()

for(link in c("logit", "probit", "cloglog"))
{
    y <- rbinom(n, size = 1, prob = 0.5)
    r <- inla(y ~ 1, data = data.frame(y),
              family = "binomial",
              control.family = list(
                  link = link))
    cat(sep="", "binomial(size=1,", link, ") ", fun(r), "\n")

    y <- rbinom(n, size = 10, prob = 0.5)
    r <- inla(y ~ 1, data = data.frame(y, m = 10),
              Ntrials = m,
              family = "binomial", 
              control.family = list(
                  link = link))
    cat(sep="", "binomial(size=10,", link, ") ", fun(r), "\n")

    y <- rbinom(n, size = 100, prob = 0.5)
    r <- inla(y ~ 1, data = data.frame(y, m = 100),
              Ntrials = m, family = "binomial", 
              control.family = list(
                  link = link))
    cat(sep="", "binomial(size=100,", link, ") ", fun(r), "\n")

    y <- rbinom(n, size = 1, prob = 0.5)
    r <- inla(y ~ 1, data = data.frame(y),
              family = "betabinomial",
              control.family = list(
                  hyper = list(rho = list(fixed = TRUE)), 
                  link = link))
    cat(sep="", "betabinomial(size=1,", link, ") ", fun(r), "\n")

    y <- rbinom(n, size = 10, prob = 0.5)
    r <- inla(y ~ 1, data = data.frame(y, m = 10),
              Ntrials = m, family = "betabinomial", 
              control.family = list(
                  hyper = list(rho = list(fixed = TRUE)), 
                  link = link))
    cat(sep="","betabinomial(size=10,", link, ") ", fun(r), "\n")

    y <- rbinom(n, size = 100, prob = 0.5)
    r <- inla(y ~ 1, data = data.frame(y, m = 100),
              Ntrials = m, family = "betabinomial", 
              control.family = list(
                  hyper = list(rho = list(fixed = TRUE)), 
                  link = link))
    cat(sep="", "betabinomial(size=100,", link, ") ", fun(r), "\n")
}

y <- rgamma(n, shape = 1, rate = 1)
r <- inla(y ~ 1, data = data.frame(y),
          family = "gamma",
          control.family = list(
              hyper = list(prec = list(initial = 0,
                                       fixed = TRUE))))
cat(sep="", "gamma ", fun(r), "\n")

y <- rgamma(n, shape = 1, rate = 1)
r <- inla(y ~ 1, data = data.frame(y),
          family = "gamma",
          control.family = list(
              hyper = list(prec = list(initial = 0,
                                       fixed = TRUE)),
              control.link = list(
                  model = "quantile",
                  quantile = 0.5)))
cat(sep="", "gamma(quantile) ", fun(r), "\n")

cat("\nTOTAL TIME ", Sys.time() - t.ref, "\n")
