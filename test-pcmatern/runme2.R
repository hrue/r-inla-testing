
 set.seed(1234234)
  
  ## Generate spatial locations
  nObs = 200
  loc = matrix(runif(nObs*2), nrow = nObs, ncol = 2)

  ## Generate observation of spatial field
  nu = 1.0
  rhoT = 0.2
  kappaT = sqrt(8*nu)/rhoT
  sigT = 1.0
  Sig = sigT^2*inla.matern.cov(nu = nu,
                               kappa = kappaT,
                               x = as.matrix(dist(loc)),
                               d = 2,
                               corr = TRUE)
  L = t(chol(Sig))
  u = L%*%rnorm(nObs)

  ## Construct observation with nugget
  sigN = 0.1
  y = u + sigN*rnorm(nObs)

  ## Create the mesh and spde object
  mesh = inla.mesh.2d(loc,
                      max.edge = 0.05,
                      cutoff = 0.01)
  spde = inla.spde2.pcmatern(mesh,
                             prior.rho = c(0.01, 0.05),
                             prior.sigma = c(10, 0.05))

  ## Create projection matrix for observations
  A = inla.spde.make.A(mesh = mesh,
                       loc = loc)

  ## Run model without any covariates
  idx = 1:spde$n.spde
  res = inla(y ~ f(idx, model = spde) - 1,
             data = list(y = y, idx = idx, spde = spde),
             control.predictor = list(A = A),
             verbose=TRUE, keep=TRUE)

