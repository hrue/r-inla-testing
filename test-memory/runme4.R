n <- 100000
x <- rnorm(n, sd = 0.2)
eta = -2 + x
prob <- 1/(1+exp(-eta))
y <- rbinom(n, size = 1, prob = prob)

r = inla(formula = y~1+x+
             f(idx, hyper = list(prec = list(prior = "pcprec",
                                             param = c(1, 0.01)))), 
         family = "binomial",
         data = list(y=y, x = x, idx = 1:n),
         verbose = TRUE,
         safe = FALSE, 
         inla.call = "inla.mkl.work",
         num.threads = "1:1", 
         control.predictor = list(cdf = c(0, 1)), 
         control.compute = list(save.memory = TRUE,
                                return.marginals.predictor = TRUE))

rr = inla(formula = y~1+x+
             f(idx, hyper = list(prec = list(prior = "pcprec",
                                             param = c(1, 0.01)))), 
         family = "binomial",
         data = list(y=y, x = x, idx = 1:n),
         verbose = TRUE,
         safe = FALSE, 
         inla.call = "inla.mkl.work",
         num.threads = "1:1", 
         control.predictor = list(cdf = c(0, 1)), 
         control.compute = list(save.memory = FALSE,
                                return.marginals.predictor = TRUE))
