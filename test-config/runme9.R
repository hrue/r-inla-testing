set.seed(123)

formula = y ~ 1 + x + f(jdx, model="ar1")

n = 300
y = rpois(n, exp(rnorm(n)))
idx = rep(1, n)
jdx = 1:n
kdx = 1:n
x = rnorm(n)

##inla.setOption(inla.call = INLA:::inla.call.builtin())
inla.setOption(safe = FALSE, num.threads = "1:1")
r = inla(formula, data =data.frame(y, idx, jdx, x),
         family = "poisson", 
         control.compute = list(config = TRUE), 
         selection = list(jdx = 1:10), 
         verbose=TRUE)
rr = inla(formula, data =data.frame(y, idx, jdx, x),
          family = "poisson", 
          control.compute = list(config = "lite"), 
          verbose=TRUE)
rrr = inla(formula, data =data.frame(y, idx, jdx, x),
          family = "poisson", 
          selection = list(jdx = 1:10), 
          verbose=TRUE)

object.size(r$misc$configs)
object.size(rr$misc$configs)
object.size(rrr$misc$configs)

rbind(r$misc$configs$config[[1]]$Q[1, 1:2],
      rr$misc$configs$config[[1]]$Q[1, 1:2],
      rrr$misc$configs$config[[1]]$Q[1, 1:2])

mean(abs(diag(r$misc$configs$config[[1]]$Q) -
         diag(rr$misc$configs$config[[1]]$Q)))
mean(abs(diag(r$misc$configs$config[[1]]$Q) -
         diag(rrr$misc$configs$config[[1]]$Q)))
mean(abs(diag(r$misc$configs$config[[1]]$Qprior) -
         diag(rr$misc$configs$config[[1]]$Qprior)))         
mean(abs(diag(r$misc$configs$config[[1]]$Qprior) -
         diag(rrr$misc$configs$config[[1]]$Qprior)))         
