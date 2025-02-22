n <- 20000
rho <- 0.7
s <- 0.5
x <- scale(arima.sim(n, model = list(ar = rho)))
y <- rpois(n, exp(x))
nt <- "10:2"

nc <- 2

##Sys.setenv(INLA_TRACE = "GMRFLib_gcpo_build")

inla.setOption(malloc.lib = 'je')

A <- matrix(rnorm(n*nc), nc, n)
e <- rep(0, nc)
INLA:::inla.my.update()
r <- inla(y ~ 1 + f(idx, model = "ar1",
                    extraconstr = list(A = A, e = e), 
                    hyper = list(prec = list(initial = 0,
                                             fixed = !TRUE),
                                 rho = list(initial = inla.models()$latent$ar1$hyper$theta2$to.theta(rho), 
                                            fixed = TRUE))),
          data = data.frame(y, idx = 1:n), 
          family = "poisson",
          control.compute = list(control.gcpo = list(enable = TRUE,
                                                     num.level.sets = 15, 
                                                     selection = -(1:n), 
                                                     size.max = 32, 
                                                     verbose = FALSE)), 
          control.inla = list(int.strategy = "eb",
              control.vb = list(enable = TRUE,
                                strategy = "mean")), 
          verbose = TRUE, 
          safe = FALSE, keep = FALSE, 
          num.threads = nt)

rr <- inla(y ~ 1 + f(idx, model = "ar1",
                     extraconstr = list(A = A, e = e), 
                     hyper = list(prec = list(initial = 0,
                                              fixed = !TRUE),
                                  rho = list(initial = inla.models()$latent$ar1$hyper$theta2$to.theta(rho), 
                                             fixed = TRUE))),
           data = data.frame(y, idx = 1:n), 
           family = "poisson",
           control.compute = list(control.gcpo = list(enable = TRUE,
                                                      num.level.sets = 15, 
                                                      selection = -(1:n), 
                                                      size.max = 32, 
                                                      verbose = FALSE)), 
           control.inla = list(int.strategy = "eb",
               control.vb = list(enable = TRUE,
                                 strategy = "mean")), 
           verbose = TRUE,
           safe = FALSE, keep = !FALSE, 
           num.threads = nt,
           inla.call = "inla.mkl.work")

r$cpu.intern
rr$cpu.intern

print(mean(abs(r$gcpo$gcpo - rr$gcpo$gcpo)))
print(max(abs(r$gcpo$gcpo - rr$gcpo$gcpo)))
