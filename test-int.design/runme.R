
n = 300
r = inla(y ~ 1,  data = data.frame(y=rnorm(n)),
         control.inla = list(
             int.strategy = "auto",
             int.design = 1), 
         verbose=T, keep=T)

r = inla(y ~ -1 + f(idx),  data = data.frame(y=rnorm(n), idx=1:n), 
         control.inla = list(
             int.strategy = "user.std",
             int.design = cbind(rnorm(n), rnorm(n), 1)), 
         verbose=T)

