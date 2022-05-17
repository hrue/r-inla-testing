## build groups using AR1

INLA:::inla.my.update()

n <- 10
rho <- 0.9
s <- 0.1
x <- scale(arima.sim(n, model = list(ar = rho)))
y <- x + rnorm(n, sd = s)

S <- toeplitz(rho^(0:(n-1)))
QQ <- solve(S) + diag(n)
SS <- solve(QQ)
CC <- SS
for(i in 1:n) for(j in 1:n) CC[i, j] <- SS[i, j]/sqrt(SS[i, i] * SS[j, j])

xx <- rnorm(n)
yy <- rnorm(n)
r <- inla(y ~ 1 + xx*yy + f(idx, model = "ar1",
                     hyper = list(prec = list(initial = 0,
                                              fixed = TRUE),
                                  rho = list(initial = inla.models()$latent$ar1$hyper$theta2$to.theta(rho), 
                                             fixed = TRUE))),
          data = data.frame(y, idx = 1:n, xx, yy), 
          family = "gaussian",
          control.family = list(hyper = list(prec = list(initial = log(1/s^2),
                                                         fixed = TRUE))),
          inla.mode = "experimental",
          control.compute = list(control.gcpo = list(enable = TRUE,
                                                     group.size = 2,
                                                     strategy = "prior",
                                                     keep = c("xx:yy", "xx"), 
                                                     verbose = TRUE)),
          control.inla = list(int.strategy = "eb"),
          verbose = TRUE,
          safe = FALSE, 
          num.threads = "1:1", 
          inla.call = "inla.mkl.work")
r$gcpo$groups
