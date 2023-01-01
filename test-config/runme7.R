set.seed(1234)
n <- 100
x <- rnorm(n, sd = 1)
eta <-  1 + x
s <- 1
y <- eta + rnorm(n, sd = s)

y[n %/% 2] <- NA
inla.setOption(inla.call = "inla.mkl.work")
r <- inla(y  ~ 1 + x,
          data = data.frame(y, x),
          control.compute = list(config = TRUE),
          verbose = T, 
          control.family = list(
              hyper = list(prec = list(initial = log(1/s^2),
                                       fixed = TRUE))))

rr <- inla(y  ~ 1 + x,
          data = data.frame(y, x),
          control.compute = list(config = TRUE),
          control.inla = list(cmin = 0), 
          family = "t")

           
