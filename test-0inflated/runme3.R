inla.setOption(verbose = TRUE,
               keep = FALSE,
               safe = TRUE,
               num.threads = "4:1")
 
sim.binomial <- function(prob, p, size) 
{
    ## - prob=zero-inflation-prob
    ## - binomial(size, p)
    stopifnot(length(prob) == length(p) && length(prob) == length(size) && length(prob) > 0)
    n <- length(prob)
    y <- numeric(n)
    event <- (runif(n) < prob)
    idx.zero <- which(event)
    idx.non.zero <- which(!event)
    y[idx.zero] <- 0
    y[idx.non.zero] <- rbinom(length(idx.non.zero), size = size[idx.non.zero], prob = p[idx.non.zero])
    return (y)
}

n <- 1000
Ntrials <- sample(1:10, n, replace = TRUE)
 
## chose link-function to use for the zero-inflation probability
link.simple <- "logit"
inv.link <- inla.link.invlogit
## link.simple <- "probit"
## inv.link <- inla.link.invprobit
## link.simple <- "cloglog"
## inv.link <- inla.link.invcloglog
 
 
prob <- rep(0.6,n)
p <- rep(0.5, n)
 
ok <- FALSE
while(!ok) {
  y <- sim.binomial(prob, p, Ntrials)
  ok <- !all(y == 0)
}
 
null.matrix = matrix(nrow = n, ncol= 0)
p.mat = null.matrix
 
rr <- inla(
  inla.mdata(cbind(y, Ntrials), p.mat) ~ 1 ,
  family = "0binomialS",
  data = data.frame(y, Ntrials),
  control.fixed = list(prec = 1,
                       prec.intercept = 1),
  control.compute = list(cpo = TRUE),
  ## in this case we need to define link.simple as the main link
  control.family = list(control.link = list(model = link.simple),
                        hyper = list(beta1 = list(param = c(0, 1)),
                                     beta2 = list(param = c(0, 1)),
                                     beta3 = list(param = c(0, 1)),
                                     beta4 = list(param = c(0, 1)),
                                     beta5 = list(param = c(0, 1)))))
