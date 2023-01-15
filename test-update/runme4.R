n <- 20
y1 <- rnorm(n)
y2 <- rnorm(n)
prec.intercept <- 1

r = inla(y ~ 1,
         data = data.frame(y=y1),
         control.fixed = list(prec.intercept = prec.intercept), 
         control.family = list(param = c(1, 1)),
         verbose = T)

r = inla(y ~ 1,
         data = data.frame(y=y2),
         control.fixed = list(prec.intercept = 1/r$summary.fixed["(Intercept)", "sd"]^2), 
         ##control.fixed = list(prec.intercept = prec.intercept), 
         control.update = list(result = r))

## do all data in one chunk
rr = inla(y ~ 1,
          data = data.frame(y=c(y1, y2)), 
          control.fixed = list(prec.intercept = prec.intercept), 
          control.family = list(param = c(1, 1)))

## compare results
dev.new()
plot(r$internal.marginals.hyperpar[[1]],
     main="Comparing joint and sequencial")
lines(rr$internal.marginals.hyperpar[[1]])
