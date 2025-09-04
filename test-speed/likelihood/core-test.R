trace.output <- FALSE
set.seed(012345)
n <- 10^7
inla.setOption(verbose = FALSE, num.threads = "1:8", safe = FALSE, verbose = FALSE)
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
          family = "poisson", keep = T)
##control.numa = list(enable = FALSE))
cat(sep="", "poisson ", fun(r), "\n")

t.ref <- Sys.time()

for(link in "logit")
{
    y <- rbinom(n, size = 1, prob = 0.5)
    r <- inla(y ~ 1, data = data.frame(y),
              family = "binomial",
              control.family = list(
                  link = link))
    cat(sep="", "binomial(size=1,", link, ") ", fun(r), "\n")
}

