n <- 50
rho <- 0.9
s <- 0.1
x <- scale(arima.sim(n, model = list(ar = rho)))
y <- x + rnorm(n, sd = s)

r <- inla(y ~ -1 + f(idx, model = "ar1",
                     hyper = list(prec = list(initial = 0,
                                              fixed = TRUE),
                                  rho = list(initial = inla.models()$latent$ar1$hyper$theta2$to.theta(rho), 
                                             fixed = TRUE))),
          data = data.frame(y, idx = 1:n), 
          family = "gaussian",
          control.family = list(hyper = list(prec = list(initial = log(1/s^2),
                                                         fixed = TRUE))),
##          control.compute = list(control.gcpo = list(enable = TRUE,
##                                                     groups = g, 
##                                                     verbose = TRUE)),
          control.inla = list(int.strategy = "eb"),
          verbose = !TRUE,
          safe = FALSE, 
          num.threads = "1:1")

m <- 10
g <- vector('list', m)
for(i in 1:m) g[[i]] <- c(i, i+1)

rr <- inla.group.cv(r, groups = g)
rr$cv

