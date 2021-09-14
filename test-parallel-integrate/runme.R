n = 100
y = rnorm(n)
x = 1:n

formula = y ~ x

r = inla(formula, data = data.frame(x,y), inla.call="inla.work", verbose=T, num.threads=100)
