INLA:::inla.my.update()
if (!exists("n")) {
    n <- 1e6
    x <- rnorm(n, sd = 0.2)
    eta = -1 + x
    alpha = 2
    lambda = exp(eta)
    y <- rweibull(n = n,shape = alpha,scale = exp(-eta/alpha))
}
r = inla(formula = y~1+x,
         family = "weibull",
         control.family = list(list(variant = 0)),
         data = list(y=y, x = x),
         verbose = TRUE,
         safe = FALSE, 
         ##control.inla = list(tolerance = 1e-6), 
         ##control.compute = list(dic = T), 
         inla.call = "inla.mkl.work",
         num.threads = "8:1", 
         control.compute = list(save.memory = TRUE,
                                return.marginals.predictor = TRUE), 
         control.expert = list(dot.product.gain = TRUE))
