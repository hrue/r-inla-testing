n = 1000
beta = 2
x = rnorm(n)
prec.x = 100
prec.y = 1000
s = runif(n)
x.tilde = x + rnorm(n, sd = 1/sqrt(s*prec.x))
y = 1 + beta * x.tilde + rnorm(n, sd = 1/sqrt(prec.y))

r = inla(y ~ f(x, model="meb", scale = s, range = c(-10, 3)), 
        family = "gaussian", 
        data = data.frame(y, x, s))
rr = inla(y ~ f(x, model="meb", scale = s, range = c(0, 0)), 
        family = "gaussian", 
        data = data.frame(y, x, s))
plot(inla.smarginal(r$marginals.hyperpar[[2]]), type="l")
lines(inla.smarginal(rr$marginals.hyperpar[[2]]))

