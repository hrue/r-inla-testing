library(INLA)
n <- 300
x <- rnorm(n, sd = 0.4)
eta <- 1 + x
p <- 1/(1+exp(-eta))
s <- runif(n, min = 0.01, max = 0.05)
p.hat <- p + rnorm(n, sd = s)

r <- inla(y ~ 1 + x, 
          data = data.frame(y = p.hat, x, s),
          family = "gaussian", 
          scale = 1/s^2, 
          control.family = list(control.link = list(model = "logit"),
                                hyper = list(prec = list(initial = 0, fixed = TRUE))), 
          verbose = TRUE)
summary(r)
