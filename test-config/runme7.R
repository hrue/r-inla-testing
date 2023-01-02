set.seed(1234)
n <- 20
x <- rnorm(n, sd = 1)
eta <-  1 + x
y <- rpois(n, exp(eta))

y[n %/% 2] <- NA
r <- inla(y  ~ 1 + x,
          data = data.frame(y, x),
          family = "poisson", 
          control.compute = list(config = TRUE))
ll <- r$misc$configs$config[[1]]$ll.info

gr <- function(x, y) {
    h <- 1e-5
    (dpois(y, exp(x +h), log = TRUE) -
     dpois(y, exp(x -h), log = TRUE)) / 2 / h
}

g <- numeric(n)
for(i in 1:n)
    g[i] <- gr(r$mode$x[i], y[i])

ggr <- function(x, y) {
    h <- 1e-5
    (dpois(y, exp(x +h), log = TRUE) -
     2 * dpois(y, exp(x), log = TRUE) +
     dpois(y, exp(x -h), log = TRUE)) / h^2
}

gg <- numeric(n)
for(i in 1:n)
    gg[i] <- ggr(r$mode$x[i], y[i])

head(
    round(dig = 6,
          cbind(est.grad = g, grad = ll[, 1], err.grad = abs(g-ll[, 1]), 
                est.hess = gg, hess = ll[, 2], err.hess = abs(gg-ll[, 2]))))

