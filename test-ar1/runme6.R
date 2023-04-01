library(mvtnorm)
n <- 3000
cor <- 0.7
phi <- 0.9
S <- matrix(c(1, cor, cor, 1), 2, 2)
eps <- rmvnorm(n, sigma = S)

x <- matrix(NA, n, 2)
x[1, ]<- eps[1, ]
for(i in 2:n) {
    x[i, ] <- phi * x[i-1, ] + eps[i, ]
}
x <- x * sqrt(1-phi^2)

age <- rnorm(n)
s <- 0.1
y <- 1 + x[, 1] + age * x[, 2] + rnorm(n, sd = s)

r <- inla(
    y ~ 1 + f(idx, model = "iid2d", n = 2,
              group = time, control.group = list(model = "ar1")) +
        f(idx.age, age, copy = "idx", group = time),
    data = data.frame(y,
                      idx = rep(1, n),
                      idx.age = rep(2, n),
                      time = 1:n), 
    family = "gaussian",
    control.family = list(
        hyper = list(prec = list(initial = log(1/s^2),
                                 fixed = TRUE))))

summary(r)
