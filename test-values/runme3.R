n <- 10
y <- rep(NA, n)
y[1] <- 1
idx <- 1:n
fun <- function(x) log((1 + x) / (1 - x))
rho <- 0.9

r <- inla(y ~ -1 + f(idx,
                     model = 'ar1',
                     hyper =  list(prec = list(initial = 0,
                                               fixed = TRUE),
                                   rho = list(initial = fun(rho),
                                              fixed = TRUE))),
          data = data.frame(y, idx),
          family = "gaussian",
          control.family = list(hyper = list(prec = list(initial = 15,
                                                         fixed = TRUE))))
          
plot(1:n, rho^(1:n -1), type = "l")
points(1:n, r$summary.random$idx$mean)

## lets tweak idx
idx <- c(1, 30, 50, 60, 70, 80, 90, 100, 110, 120)
r <- inla(y ~ -1 + f(idx,
                     model = 'ar1',
                     hyper =  list(prec = list(initial = 0,
                                               fixed = TRUE),
                                   rho = list(initial = fun(rho),
                                              fixed = TRUE))),
          data = data.frame(y, idx),
          family = "gaussian",
          control.family = list(hyper = list(prec = list(initial = 15,
                                                         fixed = TRUE))))
          
## then the results are the same
inla.dev.new()
plot(1:n, rho^(1:n -1), type = "l")
points(1:n, r$summary.random$idx$mean)
## meaning that the model is defined on 1:n, so idx are mapped into 1:n

## to avoid surprise/this from happening, use 'values', which defines where the model is
## defined. Since its 'ar1' then its on 1, 2, 3, 4, ..., n
n <- 11
idx <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11) ## this does not contain 10!
r <- inla(y ~ -1 + f(idx,
                     model = 'ar1',
                     ## say where the ar1-model is defined,  not all values need to be used
                     values = 1:n, 
                     hyper =  list(prec = list(initial = 0,
                                               fixed = TRUE),
                                   rho = list(initial = fun(rho),
                                              fixed = TRUE))),
          data = data.frame(y, idx),
          family = "gaussian",
          control.family = list(hyper = list(prec = list(initial = 15,
                                                         fixed = TRUE))))
          
## then the results are the same
inla.dev.new()
plot(1:n, rho^(1:n -1), type = "l")
points(1:n, r$summary.random$idx$mean)
## so idx are mapped into 1:n.
