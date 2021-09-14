set.seed(123)
n = 1000
x = rnorm(n)
y = rpois(n, lambda = exp(x))
E = rep(1,n)

xx = rnorm(n)
yy = rnorm(n, mean = xx)

Y = matrix(NA, 2*n, 2)

Y[1:n,1] = y
Y[(1:n)+n, 2] = yy
X = c(x,xx)
EE = c(E, rep(1,n))

formula = Y ~ X + 1

r = inla(formula, data = data.frame(X,Y),
        family = c("nbinomial", "gaussian"),
        control.family = list(list(), list(control.mix = list(model="gaussian"))), 
        E = EE, verbose=TRUE,
        control.inla = list(tolerance = 1e-6))
