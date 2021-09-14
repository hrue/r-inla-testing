n = 20
y = rnorm(n)
yy = rnorm(n)

y[1:3] = NA
yy[n:n] = NA

Y = matrix(NA, 2*n, 2)
Y[1:n, 1] = y
Y[n + 1:n, 2] = yy

r = inla(Y ~1,  data = list(Y=Y),  family = rep("gaussian", 2),
         verbose = TRUE, 
         control.compute = list(dic = TRUE))

