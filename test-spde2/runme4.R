INLA:::inla.my.update()
INLA:::inla.my.update()

n = 100
field.fcn = function(loc) (10*cos(2*pi*2*(loc[,1]+loc[,2])))
loc = matrix(runif(n*2),n,2)
idx.y = rep(1:n,2)
s <- 0.1
y = field.fcn(loc[idx.y,]) + rnorm(length(idx.y), sd=s)
mesh = inla.mesh.create(loc, refine=list(max.edge=0.05))
spde = inla.spde2.matern(mesh)
data = list(y=y, field=mesh$idx$loc[idx.y])
formula = y ~ -1 + f(field, model=spde)
r = inla(formula,
         data=data,
         family="normal",
         control.family = list(hyper = list(prec = list(initial = log(1/s^2), fixed = TRUE))), 
         inla.call = "inla.mkl.work",
         inla.arg = "-v -b -t4 -P", 
         keep = TRUE, 
         verbose=TRUE)

     
