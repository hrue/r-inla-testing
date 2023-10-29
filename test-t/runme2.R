n <- 99
x <- rnorm(n)
y <- rpois(n, exp(1 + 0.2*x + rnorm(n, sd = 0.3))) 

INLA:::inla.my.update()
r <- inla(y ~ 1+ f(idx, model = "iid"), 
          data = data.frame(y, x, idx = 1:n),
          family = "poisson",
          control.fixed = list(prec.intercept = 1, prec = 1))
rr <- inla(y ~ 1+ f(idx, model = "iid"), 
           data = data.frame(y, x, idx = 1:n),
           family = "poisson",
           control.fixed = list(prec.intercept = 1, prec = 1), 
           verbose=TRUE, 
           control.inla = list(cmin = -Inf, b.strategy = "keep",
                               hessian.correct.skewness.only = TRUE),
           control.mode = list(result = r, restart = TRUE), 
           safe = FALSE, 
           inla.call = "inla.mkl.work")

plot(inla.smarginal(r$internal.marginals.hyperpar[[1]]), lwd = 3, col = "blue", type = "l")
lines(inla.smarginal(rr$internal.marginals.hyperpar[[1]]), lwd = 3, col = "red")
abline(v=r$mode$theta, col="blue",lwd=3)
abline(v=rr$mode$theta, col="red",lwd=3)
