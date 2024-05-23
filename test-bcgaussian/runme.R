boxcox.core <- function(y, lambda) (y^lambda -1) / lambda
boxcox <- function(y, m, lambda) boxcox.core(y, lambda) - boxcox.core(m, lambda)
boxcox.inv <- function(y, m, lambda)  (m^lambda + y*lambda)^(1/lambda)
 
n <- 10^4
lambda <- 1.3
x <- scale(rnorm(n))
s <- 1.0
intercept <- 0
eta <- intercept + x
y <- eta + rnorm(n, sd = s)
yy <- inla.mdata(boxcox.inv(y, 6, lambda), 6+1, 1)

r <- inla(yy ~ 1 + x, 
          family = "bcgaussian",
          control.family = list(hyper = list(prec = list(param = c(100, 100)),
                                             lambda = list(param = c(1, 1)))), 
          data = list(yy = yy, x = x), 
          control.compute = list(cpo = T),
          control.inla = list(cmin = 0), 
          verbose = TRUE)
graphics.off()
plot(r, plot.predictor = F)
summary(r)
