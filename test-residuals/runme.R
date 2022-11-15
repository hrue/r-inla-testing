n = 2000
if (!exists("y")) {
    x <- rnorm(n, sd = 0.3)
    eta = 2 + x
    s <- 0.01
    y = eta + rnorm(n, sd = s)
}
inla.setOption(inla.call = "inla.mkl.work")


r = inla(y ~ 1 + x, 
         control.fixed = list(mean.intercept = log(mean(y)), prec.intercept = 1, prec = 1), 
         data = data.frame(y, x), 
         family = "t", 
         control.family = list(hyper = list(prec = list(param = c(s^2, 1),
                                                        initial = log(1/s^2),
                                                        fixed = TRUE))), 
         control.predictor = list(hyper = list(prec = list(initial = 20))), 
         control.compute = list(residuals = TRUE), 
         ##control.inla = list(strategy = "gaussian"), 
         inla.mode = "classic",
         verbose = T)
r <- inla.rerun(r)

rr = inla(y ~ 1 + x, 
         control.fixed = list(mean.intercept = log(mean(y)), prec.intercept = 1, prec = 1), 
         family = "t", 
         data = data.frame(y, x), 
         control.family = list(hyper = list(prec = list(param = c(s^2, 1),
                                                        initial = log(1/s^2),
                                                        fixed = TRUE))), 
         control.compute = list(residuals = TRUE), 
         ##control.inla = list(strategy = "gaussian"), 
         inla.mode = "experimental",
         verbose = T)
rr <- inla.rerun(rr)

m <- rr$summary.linear.predictor$mean
sd <- rr$summary.linear.predictor$sd
res <- sqrt(y^2 - 2*y*m + (sd^2 + m^2))/s

round(dig = 4, cbind(classic = r$dic$local.dic.sat,
                     experimental = rr$dic$local.dic.sat))
round(dig = 4, cbind(classic = r$residuals$deviance.residuals,
                     experimental = rr$residuals$deviance.residuals,
                     true = res * sign(rr$residuals$deviance.residuals)))

