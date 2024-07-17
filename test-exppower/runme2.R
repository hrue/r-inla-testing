library(gnorm)
INLA:::inla.my.update(b = T)

n <- 10^5
x <- rnorm(n)
sigma <- 2.0
beta <- 1.5
alpha <- sqrt(gamma(1/beta)/gamma(3/beta)) * sigma
## this is the lin.pred for the quantile
eta.q <- 1 + x  
quantile <- 0.9
## this is the mu/mean/median-parameter in the qgnorm
mu <- eta.q - qgnorm(quantile, alpha = alpha, beta = beta)
y <- rgnorm(n, mu = mu, alpha = alpha, beta = beta)

r <- inla(y ~ 1 + x,
          data = data.frame(y, x),
          family = "exppower",
          control.family = list(hyper = list(power = list(initial = log(beta-1), fixed = !TRUE),
                                             prec = list(initial = log(1/sigma^2), fixed =
                                                                                       !TRUE)),
                                control.link = list(model = "quantile", quantile = quantile)), 
          control.fixed = list(prec.intercept = 1), 
          control.inla = list(cmin = 0, int.strategy = "eb"), 
          ##num.threads = "1:1", 
          verbose = TRUE,
          safe = FALSE)
summary(r)

