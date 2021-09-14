
n = 10
m1 = 2
m2 = 3

y = rnorm(n)
X1 = matrix(rnorm(n*m1), n, m1)
X2 = matrix(rnorm(n*m2), n, m2)

r = inla(inla.mdata(y, X1, X2) ~ 1,
         data = list(y = y,
                     X1 = X1,
                     X2 = X2), 
         family = "bgev",
         verbose = TRUE)

