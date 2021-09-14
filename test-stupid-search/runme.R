n = 1000
y = rnorm(n)
idx = 1:n

formula = y ~ 1 + f(idx)

r = inla(formula,  data = data.frame(y, idx),  inla.call="inla.work",  keep=TRUE,
        control.fixed = list(prec.intercept = 1), 
        verbose=TRUE)
