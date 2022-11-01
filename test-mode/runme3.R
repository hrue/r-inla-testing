INLA:::inla.my.update()
inla.setOption(inla.call = "inla.mkl.work")
inla.setOption(num.threads = "1:1")
inla.setOption(inla.mode = "classic")

n=100
x=rnorm(n)
y=rpois(n,exp(x))

r=inla(y ~ 1 + x + f(idx), data=data.frame(y,x, idx = 1:n),
       family="poisson", safe = F, 
       control.predictor=list(compute=TRUE),
       control.compute=list(return.marginals.predictor=TRUE),
       verbose=T)
