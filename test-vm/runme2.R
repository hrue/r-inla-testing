library(circular)

INLA:::inla.my.update(b = T)

lfun <- function(x) 2 * atan(x)
lfuninv <- function(x) tan(x/2)

n <- 3
x <- rnorm(n, sd = 0.1)
x <- x - mean(x)
mu <- lfun(x)
kappa <- 10
y <- numeric(n)
y[1] <- -0.1
y[2] <- 0
y[3] <- 0.1

r <- inla(y ~ -1 + x,
          data = data.frame(y, x),
          family = "lavm",
          control.inla = list(cmin = 0), 
          control.family = list(link = "tan",
                                hyper = list(
                                    prec = list(initial = log(kappa),
                                                fixed = TRUE))), 
          verbose = TRUE)

