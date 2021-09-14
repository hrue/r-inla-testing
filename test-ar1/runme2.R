n = 5000
x = scale(arima.sim(n, model=list(ar=0.9)))
y = 1 + x + rnorm(n, sd = 0.1)
r = inla(y ~ -1 + f(idx, model="ar1",
                    hyper = list(mean = list(fixed=FALSE))), 
         data = data.frame(y, idx=1:n),
         family = "gaussian",
         control.family = list(
             hyper = list(
                 prec = list(
                     prior = "pc.prec",
                     param = c(3, 0.01),
                     initial = log(1/0.1^2),
                     fixed=TRUE))), 
         verbose=TRUE, keep=T)
summary(r)

