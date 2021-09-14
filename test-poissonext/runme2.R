n = 1000
E1 = runif(n)
E2 = runif(n)
EE = runif(n)
x = runif(n, min=0,max=1)

lambda = E1 + 2*E2 + EE * exp(-3 + x)
y = rpois(n, lambda)

inla.setOption("inla.call", "inla.work")
##inla.my.update()

formula = y ~ f(x) + 1
r = inla(formula, data = data.frame(x,y), family = "poisson",
        control.fixed = list(prec = 0),
        E = EE, verbose=TRUE)

