library(cubature)
lambda = 10
p = inla.pc.cormat.p2dim(3)
eps = 0.0001
r = (adaptIntegrate(inla.pc.cormat.dtheta,
                    lowerLimit = rep(0+eps, p),
                    upperLimit = rep(pi-eps/2, p),
                    tol = 1e-5,
                    maxEval = 100^p, 
                    lambda = lambda))
print(paste("TEST 1: should be 1. result = ", r$integral))
