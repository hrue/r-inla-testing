n = 25
xx = rnorm(n)
yy = rev(xx)
z = runif(n)
y = rnorm(n)

r = inla(y ~ 1 + z + f(xx) + f(yy, copy="xx"),
        data = data.frame(y, z, xx, yy), 
        control.compute = list(config=TRUE),
        family = "gaussian")

x = inla.posterior.sample(100, r)

fun = function(...) {
    mean(xx) - mean(yy)
}
f1 = inla.posterior.sample.eval(fun, x, return.matrix=TRUE)

fun = function(...) {
    c(exp(Intercept), exp(Intercept + z))
}
f2 = inla.posterior.sample.eval(fun, x, return.matrix=TRUE)

fun = function(...) {
    return (theta[1]/(theta[1] + theta[2]))
}
f3 = inla.posterior.sample.eval(fun, x, return.matrix=TRUE)


