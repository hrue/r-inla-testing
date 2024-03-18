n <- 10^4
rho <- 0.85
s <- 1
x <- scale(arima.sim(n, model = list(ar = rho)))
y <- x + rnorm(n, sd = s)
nt <- "1:1"

##cat("FIX y[]=0\n")
##y[] <- 0


if(F) r <- inla(y ~ 1 + xx + f(idx, model = "ar1",
                     hyper = list(prec = list(initial = 0,
                                              fixed = TRUE),
                                  rho = list(initial = inla.models()$latent$ar1$hyper$theta2$to.theta(rho), 
                                             fixed = TRUE))),
          data = data.frame(y, idx = 1:n, xx), 
          family = "gaussian",
          control.family = list(hyper = list(prec = list(initial = log(1/s^2),
                                                         fixed = TRUE))),
          control.compute = list(cpo = T,
                                 control.gcpo = list(enable = TRUE,
                                                     num.level.sets = 1,
                                                     selection = seq(1, n), 
                                                     verbose = !TRUE)),
          ##control.inla = list(int.strategy = "eb"),
          verbose = TRUE, 
          safe = FALSE, 
          num.threads = nt)

INLA:::inla.my.update()
rr <- inla(y ~ 1 + xx + f(idx, model = "ar1",
                     hyper = list(prec = list(initial = 0,
                                              fixed = TRUE),
                                  rho = list(initial = inla.models()$latent$ar1$hyper$theta2$to.theta(rho), 
                                             fixed = TRUE))),
          data = data.frame(y, idx = 1:n, xx), 
          family = "gaussian",
          control.family = list(hyper = list(prec = list(initial = log(1/s^2),
                                                         fixed = TRUE))),
          control.compute = list(cpo = T,
                                 control.gcpo = list(enable = TRUE,
                                                     num.level.sets = 5,
                                                     selection = seq(1, n), 
                                                     verbose = !TRUE)),
          ##control.inla = list(int.strategy = "eb"),
          verbose = TRUE,
          safe = FALSE, keep = !T, 
          num.threads = nt, inla.call = "inla.mkl.work")

r <- inla(y ~ 1 + xx + f(idx, model = "ar1",
                     hyper = list(prec = list(initial = 0,
                                              fixed = TRUE),
                                  rho = list(initial = inla.models()$latent$ar1$hyper$theta2$to.theta(rho), 
                                             fixed = TRUE))),
          data = data.frame(y, idx = 1:n, xx), 
          family = "gaussian",
          control.family = list(hyper = list(prec = list(initial = log(1/s^2),
                                                         fixed = TRUE))),
          control.compute = list(cpo = T,
                                 control.gcpo = list(enable = TRUE,
                                                     num.level.sets = 5,
                                                     selection = -seq(1, n), 
                                                     verbose = !TRUE)),
          ##control.inla = list(int.strategy = "eb"),
          verbose = TRUE,
          safe = FALSE, keep = !T, 
          num.threads = nt, inla.call = "inla.mkl.work")

rr$cpu.intern
r$cpu.intern

plot(rr$cpo$cpo, rr$gcpo$gcpo, pch = 19, cex = 2)
abline(a=0,b=1, lwd = 2)

##round(dig=6, cbind('r$gcpo' = exp(r$gcpo$gcpo), 'r$cpo' = r$cpo$cpo, 'rr$gcpo' = rr$gcpo$gcpo))
