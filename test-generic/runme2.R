n = 100
Q = INLA:::inla.rw2(n, scale.model = TRUE)
i = 1:n
y = scale((i-n/2)^2) + rnorm(n)

r = inla(y ~ f(i, model="rw2", diagonal = 1e-5, param = c(1, 1), scale.model = TRUE, constr=TRUE) + 1,
         family = "stdnormal", 
         data = data.frame(i,y),
         control.compute = list(config = TRUE), 
         control.fixed = list(prec.intercept = 0.0001), verbose = T)

rr = inla(y ~ f(i, model="generic", Cmatrix=Q,
                rankdef=2, diagonal = 1e-2, param=c(1, 1), constr=TRUE) + 1,
          family = "stdnormal", 
          data = data.frame(i,y),
          control.compute = list(config = TRUE), 
          control.fixed = list(prec.intercept = 0.0001), verbose = T)


xx=inla.posterior.sample(1, r)
fun <- function() i
x.sample <- inla.posterior.sample.eval(fun, xx)
mean(x.sample)

xx=inla.posterior.sample(1, rr)
fun <- function() i
x.sample <- inla.posterior.sample.eval(fun, xx)
mean(x.sample)

