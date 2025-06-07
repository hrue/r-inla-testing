library(circular)

lfun <- function(x) 2 * pi * (1/(1+exp(-x)) - 0.5)
lfun <- function(x) 2 * pi * (1/(1+exp(-x))) - pi
lfuninv <- function(x) log((pi + x)/(pi - x))

n <- 300
x <- rnorm(n, sd = 0.2)
x <- x - mean(x)
mu <- lfun(0.1 + x)
kappa <- 10
y <- numeric(n)
for(i in 1:n) {
    y[i] <- rvonmises(1, circular(mu[i]), kappa)
}
r <- inla(y ~ 1 + x,
          data = data.frame(y, x),
          family = "vm",
          control.inla = list(cmin = 0), 
          control.family = list(link = "tan.pi",
                                hyper = list(
                                    prec = list(initial = log(kappa),
                                                fixed = !TRUE))), 
          verbose = TRUE)
summary(r)

