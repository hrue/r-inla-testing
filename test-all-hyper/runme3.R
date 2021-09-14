n = 100
x = rnorm(n)
y = x +  rnorm(n, sd = 0.1)

r = inla(y ~ 1 + f(x, model="linear"), data = data.frame(y, x), keep=TRUE)
plot(r, plot.prior=TRUE, debug=TRUE)

r = inla(y ~ 1 + x, data = data.frame(y, x), keep=TRUE)
plot(r, plot.prior=TRUE, debug=TRUE)
