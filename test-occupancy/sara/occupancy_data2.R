
# this is the same data that is analysed in
# https://urldefense.com/v3/__https://www.jeffdoser.com/files/spoccupancy-web/__;!!Nmw4Hv0!3f3Og5e2ej7OwtZG-AA_CjWTtp6WL-CS7HJbp1afttrUfNpGzYwrG6mdVviqC2ktP_Xk3x9AxovqF49-p7jR9R__MpBf$ 

if(0)
{
  library(spOccupancy)

  data(hbef2015)
  sp.names <- dimnames(hbef2015$y)[[1]]
  
  
  # observed detection/non-detection
  Y <- hbef2015$y[sp.names == "BTBW", , ]
  #write.table(Y, file = "~/Desktop/Y.dat")
  
  
  # covariates for the detection part (this is the one that goes in the 
  # inla.mdata() function)
  X = cbind(hbef2015$det.covs[[1]][,1], hbef2015$det.covs[[2]][,1],
            hbef2015$det.covs[[1]][,2], hbef2015$det.covs[[2]][,2],
            hbef2015$det.covs[[1]][,3], hbef2015$det.covs[[2]][,3])
  
  write.table(X, file = "~/Desktop/X.dat")
  # covariates for the occurrence part 


  Xocc = hbef2015$occ.covs
  #write.table(Xocc, file = "~/Desktop/Xocc.dat")
}

rm(list = ls())
Y = read.table("~/Desktop/Y.dat")
X = read.table("~/Desktop/X.dat")
Xocc  = read.table("~/Desktop/Xocc.dat")
coord = read.table("~/Desktop/coord.dat")/1000



plot(coord)

library(INLA)
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

formula <- inla.mdata(Y,X) ~ 
  -1 + Int_occ +  scale_elev + 
  f(spatialfield, model=spde) 

