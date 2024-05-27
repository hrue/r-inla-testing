library(INLA)
library(fmesher)

INLA:::inla.my.update(b = T)
inla.setOption(num.threads = 4)

Y = read.table("Y.dat")
X = read.table("X.dat")
Xocc  = read.table("Xocc.dat")
coord = read.table("coord.dat")/1000

ss.list <- list(c(1, 3, 5), c(2, 4, 6))
for (ss in seq_along(ss.list)) {
    cols <- ss.list[[ss]]
    z <- unlist(X[, cols])
    m <- mean(z, na.rm = TRUE)
    s <- sd(z, na.rm = TRUE)
    X[, cols] <- (z-m)/s
}
X <- cbind(1, X[, 1], X[1, 2],   1, X[, 3], X[, 4],   1, X[, 4], X[, 5])

mesh = fm_mesh_2d(loc  = coord, 
                  max.edge =  0.3, 
                  offset = 0.7, cutoff = 0.3)
plot(mesh)
points(coord)

spde <- inla.spde2.pcmatern(
  mesh = mesh, 
  prior.range = c(0.5, 0.5),
  prior.sigma = c(1, 0.01)) 

iset_sp <- inla.spde.make.index(name = "spatialfield",
                                n.spde =  spde$n.spde)

A_sp <- inla.spde.make.A(mesh = mesh, 
                         loc = cbind(coord[,1], coord[,2]))

stk <- inla.stack(data=list(Y = Y,
                            X = X), #the response
                  A=list(A_sp,1),  #the A matrix; the 1 is included to make the list(covariates)
                  effects=list(c(list(Int_occ=1), #the Intercept
                                 iset_sp),  #the spatial index
                               #the covariates
                               list(scale_elev = scale(Xocc[,1]))), 
                  tag='dat')

formula <- inla.mdata(Y,X) ~ -1 + Int_occ +  scale_elev + f(spatialfield, model=spde) 

r <- inla(formula, 
          family = "occupancy",
          data = inla.stack.data(stk), 
          safe = FALSE,
          verbose = TRUE,
          control.fixed = list(prec.intercept = 1), 
          control.inla = list(cmin = 0.0))
summary(r)
cbind(beta, r$summary.hyperpar[, "mean"])

