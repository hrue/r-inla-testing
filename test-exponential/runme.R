n = 20
x = runif(n)
lambda = exp(1+x)
y = rexp(n, rate=lambda)
event = rep(1,n)
data = list(y=y, event=event, x=x)
formula = inla.surv(y,event) ~ x 
model = inla(formula, family ="exponential.surv", data=data,
             control.compute = list(dic=TRUE),
             verbose=TRUE)
summary(model)

formula = y ~ x 
model = inla(formula, family ="exponential", data=data, 
             control.compute = list(dic=TRUE), 
             verbose=TRUE)
summary(model)
