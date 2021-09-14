p.x = runif(1)
p.y = runif(1)
x=rnorm(1, sd=1/sqrt(p.x))
y=rnorm(1, mean=x, sd = 1/sqrt(p.y))
idx=1

formula = y ~ -1 + f(idx, model="iid",  initial = log(p.x), fixed=TRUE)
r = inla(formula, data = data.frame(y, idx),
        family = "gaussian", 
        control.data = list(initial = log(p.y), fixed=TRUE),
        control.compute=list(mlik=TRUE))

print(r$mlik)
print(dnorm(y, mean = 0, sd = sqrt(1/p.x + 1/p.y), log=TRUE))

