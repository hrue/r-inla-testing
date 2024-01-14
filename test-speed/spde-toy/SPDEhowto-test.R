library(lattice);   library(gridExtra);  library(INLA);  library(plyr) 
set.seed(123)


## ----locations,tidy=FALSE-----------------------------------------------------
n <- 2000; coo <- matrix(runif(2*n), n) 
s2u <- .5; k <- 10; r <- 2/k ## RF params.
R <- s2u*exp(-k*as.matrix(stats::dist(coo))) 


## ----rmvnorm,tidy=FALSE-------------------------------------------------------
u <- drop(rnorm(n)%*%chol(R)) 


## ----noise,tidy=FALSE---------------------------------------------------------
x <- runif(n);  beta <- 1:2;  s2e <- 0.2
lin.pred <- beta[1] + beta[2]*x + u 
y <- lin.pred + rnorm(n, 0, sqrt(s2e)) 


## ----fmesh, tidy=FALSE, pars=list(mar=c(0,0,0.7,0)), out.width='0.45\\textwidth', fig.align='center'----
mesh <- inla.mesh.2d(coo, cutoff=r/10, 
  max.edge=c(r/4, r/2), offset=c(r/2, r)) 
##plot(mesh, asp=1);  points(coo, col='red') 


## ----projector, tidy=FALSE----------------------------------------------------
A <- inla.spde.make.A(mesh=mesh, loc=coo)


## ----spde,tidy=FALSE----------------------------------------------------------
spde <- inla.spde2.pcmatern(
 mesh=mesh, alpha=1.5, 
 prior.range=c(0.2, 0.5),#P(range<0.2)=0.5
 prior.sigma=c(1, 0.5)) ## P(sigma>1)=0.5 


## ----stack-estimation,tidy=FALSE----------------------------------------------
stk.e <- inla.stack(tag='est', ## tag id
  data=list(y=y),  ## response
  A=list(1, A), ## two projection matrices
  effects=list(## two elements: 
    data.frame(b0=1, x=x), ## covariate
    idx.u=1:spde$n.spde)) ## RF index 


## ----fitting------------------------------------------------------------------
pcprec <- list(hyper=list(theta=list(
  prior='pc.prec',param=c(1,.1))))
mf <- y ~ 0 + b0 + x + f(idx.u, model=spde) 

r <- inla(mf,
          family = "t", 
          control.family=pcprec, 
          data=inla.stack.data(stk.e), 
          control.compute=list(return.marginals.predictor=TRUE), 
          control.inla = list(int.strategy = "eb",
                              verbose = !T, 
                              control.vb = list(enable = FALSE)), 
          control.predictor=list(
              compute=TRUE,
              A=inla.stack.A(stk.e)),
          control.mode = list(theta = c(1.4289345305,  4.9656994790, -1.8062028998, -0.6014129724),
                              restart = TRUE), 
          num.threads = 4,
          verbose = TRUE,
          safe = FALSE, 
          inla.call = "inla.mkl.work")

x <- r$mode$x
Aeta <- r$mode$x[1:2000]
eta <- r$mode$x[2001:(2001 + 3120 -1)]
u <- r$mode$x[5121:(5121-1+1881)]
beta.b0 <- r$mode$x[7002]
beta.x <- r$mode$x[7003]
A <- inla.stack.A(stk.e)

r$summary.fixed["b0", "mode"] - beta.b0
r$summary.fixed["x", "mode"] - beta.x
mean(abs(r$summary.random$"idx.u"$mode - u))
mean(abs(r$summary.linear.predictor$mode[2001:(2001 + 3120 -1)] - eta))
mean(abs(r$summary.linear.predictor$mode[1:2000] - Aeta))
