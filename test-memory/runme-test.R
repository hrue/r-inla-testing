if (!exists("n")) {
    n <- 1e6
    eta = -1
    alpha = 3
    lambda = exp(eta)
    y <- rweibull(n = n,shape = alpha,scale = exp(-eta/alpha))
}
r = inla(formula = y~1, ## + f(idx, hyper = list(prec = list(prior = "pcprec", param = c(0.5, 0.01)))), 
         family = "weibull",
         control.family = list(list(variant = 0)),
         data = list(y=y, idx = 1:n),
         verbose = TRUE,
         safe = FALSE, 
         ##control.inla = list(tolerance = 1e-6), 
         ##control.compute = list(save.memory = TRUE), 
         inla.call = INLA:::inla.call.builtin(), 
         num.threads = "4:1")
         ##control.expert = list(dot.product.gain = TRUE))

INLA:::inla.my.update(b = T)

rr = inla(formula = y~1, ## + f(idx, hyper = list(prec = list(prior = "pcprec", param = c(0.5, 0.01)))), 
         family = "weibull",
         control.family = list(list(variant = 0)),
         data = list(y=y, idx = 1:n),
         verbose = TRUE,
         safe = FALSE, 
         ##control.inla = list(tolerance = 1e-6), 
         control.compute = list(save.memory = TRUE), 
         inla.call = "inla.mkl.work",
         num.threads = "4:1")
         ##control.expert = list(dot.product.gain = TRUE))
