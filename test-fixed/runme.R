## simple example of the fixed-option in 'control.mode'. 
set.seed(123)
n = 100
x = arima.sim(n, model=list(ar=0.9))
x = x/sd(x)
y = 1 + x + rnorm(n, sd=0.01)

idx = 1:n
formula = y ~ 1 + f(idx,  model="ar1")

r = inla(formula, data = data.frame(y, idx),
        control.inla = list(int.strategy = "eb"))
rr = inla(formula, data = data.frame(y, idx),
        control.mode = list(result = r,  fixed=TRUE))


