n=30000
a = 3
b = 1
p <- 0.2
x = rnorm(n, sd = 0.3)
eta = a + b*x
interval = c(2, 3)
E = rep(1, n)
lambda = E*exp(eta)
y <- numeric(n)
for(i in 1:n) {
    if (runif(1) < p) {
        y[i] <- 0
    } else {
        while(TRUE) {
            y[i] = rpois(1, lambda = lambda[i])
            if (y[i] >0)
                break
        }
    }
}

censored = (y >= interval[1] & y <= interval[2])
y[censored] = interval[1]

Y <-  matrix(NA, n, 3)
for(i in 1:n) {
    if (i <= n/3) {
        Y[i, 1] <- y[i]
    } else if (i < 2*n/3) {
        Y[i, 2] <- y[i]
    } else {
        Y[i, 3] <- y[i]
    }
}
r = (inla(Y ~ 1 + x, 
          family = c("zeroinflatedcenpoisson0", "zeroinflatedcenpoisson0", "zeroinflatedcenpoisson0"), 
    control.family = list(list(cenpoisson.I = interval),
                          list(cenpoisson.I = interval), 
                          list(cenpoisson.I = interval)), 
    data = list(Y = Y, x = x),
    control.compute = list(cpo = TRUE), 
    verbose = T, 
    E=E))
summary(r)



