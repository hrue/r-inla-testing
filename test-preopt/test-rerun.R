n <- 10
x <- rnorm(n)
s <- 0.1
off <- (1:n)/10
y <- 1 + x + off + rnorm(n, sd = s)

r <- inla(y ~ 1 + x + offset(-off),
          data = data.frame(y, x, off = off),
          verbose = TRUE,
          family = "t", 
          control.fixed = list(prec = 1, prec.intercept = 1), 
          control.predictor = list(hyper = list(prec = list(initial = 20))), 
          control.family = list(hyper = list(prec = list(initial = log(1), fixed = FALSE),
                                             dof = list(initial = 6, fixed = TRUE))), 
          inla.call = "inla.mkl.work", inla.arg = "-v -t2:2 -b -P")





