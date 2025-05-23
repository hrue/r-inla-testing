INLA:::inla.my.update()
inla.setOption(inla.call = 'inla.mkl.work')

n <- 300
a <- 0
b <- 1
x <- rnorm(n, sd = 0.3)
eta = a + b*x
low = sample(c(0, 1, 4, Inf), n, replace = TRUE) 
high <- low + sample(c(0, 1, 2, Inf), n, replace = TRUE) 

E = sample(1:10, n, replace=TRUE)
lambda = E*exp(eta)
y = rpois(n, lambda = lambda)

r = inla(inla.mdata(y, low, high)  ~ 1 + x, 
         family = "cenpoisson2",
         data = data.frame(y, low, high, x), 
         E=E,
         control.compute = list(dic = T, cpo = T), 
         verbose = TRUE)
summary(r)
