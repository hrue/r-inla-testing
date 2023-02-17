## simple example of the fixed-option in 'control.mode'. 
set.seed(123)
n = 1000
x = arima.sim(n, model=list(ar=0.9))
x = x/sd(x)
s <- 0.1
y = 1 + x + rnorm(n, sd=s)

inla.setOption(safe = FALSE)
## inla.setOption(inla.call = "inla.mkl.work")

idx = 1:n
formula = y ~ 1 + f(idx,  model="ar1", fixed = c(TRUE, TRUE))

## optimize as usual
r = inla(formula, data = data.frame(y, idx),
         control.family = list(hyper = list(prec = list(initial = log(1/s^2),
                                                        fixed = TRUE))))

## assume mode is known, but compute the Hessian and explore the neigbourhood
rr = inla(formula, data = data.frame(y, idx), 
          control.mode = list(result = r,  restart = FALSE),
          control.family = list(hyper = list(prec = list(initial = log(1/s^2),
                                                         fixed = TRUE))))
        

## assume mode is known and fixed (or constant) NOTE: in this case the marginals for the
## hyperparameters are stil in the output but these results has no meaning.
rrr = inla(formula, data = data.frame(y, idx), 
           control.mode = list(result = r,  fixed = TRUE),
           control.family = list(hyper = list(prec = list(initial = log(1/s^2),
                                                          fixed = TRUE))))
        

print(c(r$misc$nfunc, rr$misc$nfunc, rrr$misc$nfunc))
print(rbind(r = r$mode$theta, rr = rr$mode$theta, rrr = rrr$mode$theta))
