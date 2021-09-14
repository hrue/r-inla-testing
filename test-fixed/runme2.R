n = 100
offset.x = 2
offset.xx = 5

x = rnorm(n)
xx = offset.xx + rnorm(n)
y = rpois(n, lambda = exp(1 + x + xx))

r = inla(y ~ 1 + x + xx, data = data.frame(y, x, xx),
        family = "nbinomial", 
        control.fixed = list(correlation.matrix = TRUE))

