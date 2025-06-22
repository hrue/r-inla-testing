INLA:::inla.my.update(b = T)

n <- 10000
rho <- 0.8
s <- 0.5
x <- scale(arima.sim(n, model = list(ar = rho)))
xx <- rnorm(n)
y <- 1 + xx + x + rnorm(n, sd = s)

r <- inla(y ~ 1 + xx + f(idx, model = "ar", order = 10,  
                     hyper = list(prec = list(initial = 0,
                                              fixed = TRUE))), 
          data = data.frame(y, idx = 1:n, xx), 
          family = "gaussian",
          control.family = list(hyper = list(prec = list(initial = log(1/s^2),
                                                         fixed = TRUE))),
          control.compute = list(smtp = "taucs",
                                 control.gcpo = list(enable = TRUE,
                                                     num.level.sets = 5,
                                                     verbose = !TRUE)),
          control.inla = list(int.strategy = "eb"),
          control.stiles = list(tile.size = 0), 
          num.threads = "20:1", 
          verbose = TRUE,
          inla.call = "inla.mkl", 
          safe = FALSE,
          keep = TRUE)
