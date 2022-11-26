y <- rep(NA, 15)
y[1:5] <- rpois(5, lambda = 1)

yy <- rep(NA, 15)
yy[6:10] <- rnorm(5)

yyy <- inla.surv(c(rep(NA, 10), 1:5), c(rep(NA, 10), rep(1, 5)), cure = cbind(1, 1:15))

r <- inla(Y ~ 1,
          family = c("poisson", "normal", "weibullsurv"), 
          data = list(Y = list(y = y, yy = yy, yyy = yyy)), 
          control.fixed = list(prec.intercept = 1), 
          control.family = list(list(), list(), list(variant = 1)), 
          ## will enable config=T
          control.compute = list(likelihood.info = TRUE),
          verbose = TRUE,
          safe = FALSE)

a1=eval(parse(text=r$misc$configs$config[[1]]$arg.str[1]))
a1$linear.predictor <- c(1, 2, 3)
do.call(inla.likelihood, args = a1)

a6=eval(parse(text=r$misc$configs$config[[1]]$arg.str[6]))
a6$linear.predictor <- c(1, 2, 3)
do.call(inla.likelihood, args = a6)

a11=eval(parse(text=r$misc$configs$config[[1]]$arg.str[11]))
a11$linear.predictor <- c(1, 2, 3)
do.call(inla.likelihood, args = a11)
