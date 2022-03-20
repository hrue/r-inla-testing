n <- 10^5
y <- rnorm(n)
inla.setOption(num.threads = "1:1")

r <- inla(y ~ 1,
          data = data.frame(y),
          control.fixed = list(prec.intercept = 1),
          inla.mode = "experimental",
          keep = T, 
          inla.call = "inla.mkl")
r$cpu
rr <- inla(y ~ 1,
          data = data.frame(y),
          control.fixed = list(prec.intercept = 1),
          inla.mode = "experimental",
          inla.call = "inla.mkl.work")
rr$cpu
