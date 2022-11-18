n <- 20
m <- 10
nm <- n * m
s <- seq(0.1, 2.0, len = m)
a <- 1
b <- 1
x <- rnorm(nm)
Y <- matrix(NA, nm, m)

for(mm in 1:m) {
    Y[(mm-1) * n + 1:n, mm] <- a + b * x[(mm-1)*n + 1:n] + rnorm(n, sd = s[mm])
}

r <- inla(Y ~ 1 + x,
          family = rep("gaussian", m),
          data = list(Y = Y, x = x))

summary(r)
cbind(est = 1/sqrt(r$summary.hyperpar[, "mode"]), true = s)
