n = 20
beta = 2
x = rnorm(n)
prec.x = 100
prec.y = 1000
s = runif(n)
x.tilde = x + rnorm(n, sd = 1/sqrt(s*prec.x))
y = 1 + beta * x.tilde + rnorm(n, sd = 1/sqrt(prec.y))

r = inla(y ~ f(x, model="meb", scale = s, range = c(-10, 3)), 
        family = "gaussian", 
        data = data.frame(y, x, s),
        verbose = TRUE,
        inla.call = "inla.mkl.work", 
        inla.arg = "-v -b -t2:1 -P")
rr = inla(y ~ f(x, model="meb", scale = s, range = c(-10, 3)), 
        family = "gaussian", 
        data = data.frame(y, x, s),
        verbose = TRUE, 
        inla.call = "inla.mkl.work",
        inla.arg = "-v -b -t2:1 -P")

        


