n = 100
intercept = 2
x = rnorm(n, sd = 0.2)
beta = 1
eta = intercept + beta * x
y = numeric(n)
E = runif(n, min=1, max=10)

res = c()
for(alpha in seq(0.01,  0.99, by = 0.02)) {
    for(i in 1:n) {
        lambda = E[i] * INLA:::inla.qcontpois(exp(eta[i]), alpha = alpha)
        y[i] = rpois(1, lambda)
    }
    
    r = inla(y ~ 1 + x, data = data.frame(y, x, E),
             family = "qpoisson",
             control.family = list(quantile = alpha),
             E =E)
    print(c(alpha, r$summary.fixed[, "mean"]))
    res = c(res, (c(alpha, r$summary.fixed[, "mean"])))
}

mres = matrix(res, ncol=3, byrow=T)
plot(mres[, 1], mres[, 2],  ylim = c(0, 3), xlab = "alpha", pch=19, 
     main="comparing estimates of intercept & beta for various alpha")
abline(a=intercept, b=0, lwd=3)
points(mres[, 1], mres[, 3], pch=19)
abline(a=beta, b=0, lwd=3)



