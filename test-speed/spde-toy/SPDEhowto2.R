## ----setting,include=FALSE----------------------------------------------------
library(knitr)
knit_hooks$set(pars = function(before, options, envir) {
    if (before) graphics::par(options$pars)
})
opts_chunk$set(message=FALSE, warning=FALSE, tidy=FALSE,
               fig.path='figures/SPDEhowto/')
library(lattice);   library(gridExtra);  library(INLA);  library(plyr) 
set.seed(123)
inla.setOption(num.threads="2:1")
inla.setOption(smtp="taucs")
inla.setOption(inla.mode = "experimental")
lcall <- inla.getOption('inla.call')


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
plot(mesh, asp=1);  points(coo, col='red') 


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

res.new <- inla(mf, control.family=pcprec,
                family = "normal", 
                data=inla.stack.data(stk.e), ## data 
                control.compute=list(config = TRUE,
                                     return.marginals.predictor=TRUE),
                control.predictor=list(compute=TRUE, 
                                       A=inla.stack.A(stk.e)))# full projector

plot(mesh)
dev.new()

library(rgl)
col.pal <- colorRampPalette(c("blue", "cyan", "green", "yellow", "red"))
plot(mesh,
     res.new$summary.random$idx.u[, "mean"], 
     rgl = TRUE,  add = TRUE
     )
