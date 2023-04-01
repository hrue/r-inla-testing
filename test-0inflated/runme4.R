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

n <- 100
Ntrials <- sample(1:10, n, replace = TRUE)
x <- rnorm(n, sd = 0.3)

link.simple <- "logit"
inv.link <- inla.link.invlogit
 
prob <- 1/(1+exp(-x))
p <- rep(0.5, n)
 
ok <- FALSE
while(!ok) {
  y <- sim.binomial(prob, p, Ntrials)
  ok <- !all(y == 0)
}
 
stopifnot(n>51)

y[51:n] <- NA
Y1 <- inla.mdata(cbind(y, Ntrials), p.mat) 

Y2 <- x + rnorm(n)
Y2[1:50] <- NA

Y12 <- list(Y1 = Y1, Y2 = Y2)

rr <- inla( Y12 ~ 1 + x, 
  family = c("0binomialS","gaussian"), 
  data = list(Y12 = Y12, Ntrials = Ntrials))
