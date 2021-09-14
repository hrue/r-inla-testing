require(INLA)
##inla.setOption("inla.call",  "inla.work")

globe.n = 10  ## Number of points generated = 2+10*n^2
replicates = 5 ## Number of field replicates
basis.n.T = 2  ## Number of non-stationary T/K-basis functions
basis.n.K = 3  ## Number of non-stationary T/K-basis functions
basis.degree = 2  ## B-pline degree for the basis functions
sd.y = 0.01

## Make sure the B-spline degree is in the allowed range:
basis.degree = max(0, min(max(basis.n.T, basis.n.K)-1, basis.degree))


## Generate mesh:

mesh = inla.mesh.create(globe=globe.n, refine=FALSE)
mesh = inla.mesh.create(loc=mesh$loc, refine=list(max.edge=0.08))


## Use the evenly distributed globe points as data locations:

data.idx = mesh$idx$loc
data.n = length(data.idx)


# Construct model:

basis.T = (inla.mesh.basis(mesh, type="b.spline",
                           n=basis.n.T, degree=basis.degree,
                           knot.placement="uniform.area", rot.inv=TRUE))
basis.K = (inla.mesh.basis(mesh, type="b.spline",
                           n=basis.n.K, degree=basis.degree,
                           knot.placement="uniform.area", rot.inv=TRUE))

spde = (inla.spde.create(mesh, model="matern",
                         param=(list(alpha=2,
                                     basis.T=basis.T,
                                     basis.K=basis.K
                                     ))
                         ))



## Set the true parameter values:

if (basis.n.T == 1 && basis.n.K == 1) {
    theta.T = -5
    theta.K = 5
} else if (basis.n.T == 2 && basis.n.K == 2) {
    theta.T = c(-5,-5)
    theta.K = c(5,5)
} else {
    theta.T = c(-4,rep(-6,basis.n.T-2),-4)
    theta.K = c(4,rep(6,basis.n.K-2),4)
}
tau = exp(basis.T %*% theta.T)
kappa2 = exp(basis.K %*% theta.K)


## Simulate data:

x = matrix(, mesh$n, replicates)
y = matrix(, data.n, replicates)
for (i in 1:replicates) {
    x[,i] = inla.spde.query(spde, sample=list(tau=tau, kappa2=kappa2))$sample
    y[,i] = x[data.idx,i] + rnorm(data.n, sd=sd.y)
}

##cp = colorRampPalette(c("blue","cyan","yellow","red"),space="Lab")
##plot(old.mesh.class(mesh), rowSums(x), draw.edge=FALSE, color.palette=cp)
##plot(old.mesh.class(mesh), sqrt(rowSums(x^2)), draw.edge=FALSE, color.palette=cp)



## Estimate model:

inla.data = (list(y = as.vector(y),
                  field = rep(data.idx,replicates),
                  year = rep(1:replicates, each=data.n),
                  spde.model = spde))

formula = y ~ -1 + f(field, model=spde.model, replicate=year)

result = (inla(formula, family="gaussian", data=inla.data,
               control.mode = (list(theta = c(theta.T,theta.K),
                                    restart=TRUE)),
               control.data = list(hyper = list(prec = list(
                                                        initial = log(1/sd.y^2),
                                                        fixed=TRUE))), 
               verbose=TRUE,
               control.compute =list(return.marginals=FALSE),
               keep=TRUE))
