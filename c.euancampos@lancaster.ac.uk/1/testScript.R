library(sp)
library(INLA)

control.tail.cgevit <- list(model = "cgevit",
                            hyper = list(tail = list(prior = "pcegptail",
                                                     param = c(7, -0.5, 0.5)),
                                         p0 = list(initial = 0.5,  #I replaced intercept for p0 as 'intercept' was not found
                                                   param = c(0, 1))))

p0<-0.2
b1  <- 1
xi1 <- -0.3
#set sample size
n <-300
# covariate
x   = rnorm(n, sd = .5)
# intercept 
b0  = inla.link.gevit(1-p0,tail=xi1)
# linear predictor
eta = b0 + b1 * x
# apply links to eta
prob = 1 - inla.link.invgevit(eta, tail = xi1)
# simulate observations
size = 1
set.seed(3654)
y    = rbinom(n, size = size, prob = prob)


res.cgevit = inla(y ~ -1 + x,
                  data = data.frame(y, x), 
                  family = "binomial",
                  Ntrials = size,
                  control.family = list(control.link = control.tail.cgevit),
                  control.compute = list(config = TRUE),
                  verbose = TRUE,
                  keep = T)

