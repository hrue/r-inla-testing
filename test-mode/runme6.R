## simple example of the fixed-option in 'control.mode'. 
set.seed(123)
n = 1000
x = arima.sim(n, model=list(ar=0.9))
x = 0.5 * x/sd(x)
y = rpois(n, exp(-1 + x))

inla.setOption(safe = FALSE)
inla.setOption(inla.call = "inla.mkl.work")

idx = 1:n
formula = y ~ 1 + f(idx,  model="ar1")

## optimize as usual
r = inla(formula, data = data.frame(y, idx), family = "poisson",
         control.inla = list(control.vb = list(enable = FALSE)))
rr = inla( y ~ 1 + f(idx,  model="ar1", initial = r$mode$theta, fixed = c(T, T)),
          data = data.frame(y, idx), family = "poisson")
rrr = inla(formula, 
          data = data.frame(y, idx), family = "poisson",
          control.mode = list(result = r, fixed = TRUE))


