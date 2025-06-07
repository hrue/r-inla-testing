library(INLA)
library(dplyr)
n <- 300
n2 <- 2*n

u <- rnorm(n)
eta1 <- 1 + u
eta2 <- 2 + 2*u
y1 <- eta1 + rnorm(n)
y2 <- eta2 + rnorm(n)

Y <- matrix(NA, n2, 2)
Y[1:n, 1] <- y1
Y[n + 1:n, 2] <- y2

formula <- Y ~ -1 +
    intercept +
    f(idx.u, model = "iid") +
    f(idx.u2, copy="idx.u", hyper=list(beta=list(fixed=FALSE)))

d1 <- data.frame(intercept = as.factor(rep(1, n)), idx.u = 1:n)
d2 <- data.frame(intercept = as.factor(rep(2, n)), idx.u2 = 1:n)

data <- as.list(bind_rows(d1, d2))
data$Y <- Y
str(data)

r <- inla(formula, data = data, family = rep("stdgaussian", 2))
summary(r)



