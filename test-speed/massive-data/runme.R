n <- 10^5
x <- inla.group(runif(n))
w <- rnorm(n)
y <- rpois(n, exp(1 + x))

r <- inla(y ~ 1 + f(x, w, model = "iid"),
          data = data.frame(y, x, w),
          family = "poisson", 
          control.fixed = list(prec.intercept = 1), 
          control.inla = list(int.strategy = "eb"), 
          control.compute = list(control.gcpo = list(enable = TRUE,
                                                  num.level.sets = 3)), 
          verbose = TRUE,
          keep = TRUE,
          safe = FALSE)

              
