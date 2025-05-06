n <- 100
rho <- 0.85
s <- runif(n, min = 0.25, max = 0.9)
x <- scale(arima.sim(n, model = list(ar = rho)))
y <- x + rnorm(n, sd = s)
nt <- "1:1"

weights = runif(n)
INLA:::inla.my.update(b = T)
rr <- inla(y ~ 1 +
               f(idx, model = "ar1",
                 hyper = list(prec = list(initial = 0,
                                          fixed = TRUE),
                              rho = list(initial = inla.models()$latent$ar1$hyper$theta2$to.theta(rho), 
                                         fixed = TRUE))),
           scale = 1/s^2, 
           data = data.frame(y, idx = 1:n, s), 
           family = "gaussian",
           control.family = list(hyper = list(prec = list(initial = 0, 
                                                          fixed = TRUE))),
           control.compute = list(cpo = T,
                                  control.gcpo = list(enable = TRUE,
                                                      num.level.sets = 5,
                                                      size.max = 32, 
                                                      selection = -(1:n),
                                                      group.selection = 1:n, 
                                                      weights = weights, 
                                                      verbose = !TRUE)),
           ##control.inla = list(int.strategy = "eb"),
           verbose = TRUE,
           safe = FALSE, keep = !T, 
           num.threads = nt, inla.call = "inla.mkl.work")

rr$gcpo$groups

v <- rep(0, n)
m <- rep(0, n)
for(i in 1:n) {
    v[i] <- sum(weights[rr$gcpo$groups[[i]]$idx])
    m[i] <- length(rr$gcpo$groups[[i]]$idx)
}
cbind(1:n, v, m)


cv <- inla.group.cv(rr,  num.level.sets = 2, size.max = 32, selection = 1:n, group.selection = 1:n, weights = weights)

m.err <- 0
for(i in 1:n) {
    stopifnot(all(rr$gcpo$groups[[i]]$idx ==  cv$groups[[i]]$idx))
    m.err <- max(m.err, abs(rr$gcpo$groups[[i]]$corr - cv$groups[[i]]$corr))
}
print(m.err)
