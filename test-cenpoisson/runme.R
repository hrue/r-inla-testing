n=300
a = 0
b = 1
x = rnorm(n, sd = 0.3)
eta = a + b*x
interval = c(1, 4)
E = runif(n, min = 0.1, max = 2)
lambda = E*exp(eta)
y = rpois(n, lambda = lambda)

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
          family = c("cenpoisson", "cenpoisson", "cenpoisson"), 
    control.family = list(list(cenpoisson.I = interval),
                          list(cenpoisson.I = interval), 
                          list(cenpoisson.I = interval)), 
          data = list(Y = Y, x = x),
          control.compute = list(cpo = TRUE), 
          E=E))
summary(r)
