library(knitr)
library(sn)
knit_hooks$set(pars = function(before, options, envir) {
  if (before) graphics::par(options$pars)
})
opts_chunk$set(message=FALSE, warning=FALSE, tidy=FALSE,
               fig.path='SPDEhowto-figs/SPDEhowto-')
library(lattice);   library(gridExtra);  library(INLA);  library(plyr) 

deafult_error = c();
smart_error = c()
deafult_nfunc <- c()
smart_nfunc <- c()

INLA:::inla.my.update()
inla.setOption(num.threads="8:1")
inla.setOption(inla.mode="experimental")
##inla.setOption(inla.mode="classic")
inla.setOption(inla.call = "inla.mkl.work")
##inla.setOption(inla.call = "inla.valgrind")

nfun <- 0

for(ii in 1:50)
{
    ## ----locatio3
    n <- 300
    coo <- matrix(runif(2*n), n) 
    s2u <- .5
    k <- 10; 
    r <- 2/k ## RF params.
    R <- s2u*exp(-k*as.matrix(dist(coo))) 
    
    ## ----rmvnorm
    u <- drop(t(chol(R)) %*% rnorm(n))
   
    ## ----noise
    x <- rnorm(n)
    beta <- (1:2)/2
    lin.pred <- beta[1] + beta[2]*x + u 
    eta = lin.pred
    y <- rbinom(n, size = 1, prob = 1/(1+exp(-eta)))

    ## ----fmesh
    mesh <- inla.mesh.2d(coo, cutoff=r/10, 
                         max.edge=c(r/4, r/2), offset=c(r/2, r)) 
                                        #plot(mesh, asp=1);  points(coo, col='red') 

    ## ----projector
    A <- inla.spde.make.A(mesh=mesh, loc=coo)
    
    ## ----spde
    spde <- inla.spde2.pcmatern(
        mesh=mesh, alpha=1.5, 
        prior.range=c(0.2, 0.5),#P(range<0.2)=0.5
        prior.sigma=c(1, 0.5)) ## P(sigma>1)=0.5 
    
    ## ----stack-estimation
    stk.e <- inla.stack(tag='est', ## tag id
                        data=list(y=y),  ## response
                        A=list(1, 1, A), ## two projection matrices
                        effects=list(## two elements: 
                            data.frame(b0=1, x=x), ## covariate
                            idx = 1:n, 
                            idx.u=1:spde$n.spde))

    ## ----fitting
    pcprec <- list(hyper=list(theta=list(
                                  prior='pc.prec',param=c(1,.1))))
    mf <- y ~ 0 + b0 + x +
        ##f(idx, hyper = list(prec = list(prior = "pc.prec", param = pcprec))) +
        f(idx.u, model=spde) 

    if (TRUE) {
         r3 <- inla(mf, 
                   family = "binomial",
                   data=inla.stack.data(stk.e), ## data 
                   control.predictor=list(compute=TRUE, A=inla.stack.A(stk.e)),
                   control.inla = list(int.strategy = "ccd", bfgs.version = 3), 
                   verbose = F)
    } else {
        r3 <- NULL
    }

     r4 <- inla(mf, 
              family = "binomial",
              data=inla.stack.data(stk.e), ## data 
              control.predictor=list(compute=TRUE, 
                                     A=inla.stack.A(stk.e)),
              control.inla = list(bfgs.version = 4), 
              verbose = F)
    
    if (is.null(r3)) r3 <- r4
    
    nfun <- nfun + r4$misc$nfunc
    print(round(dig = 3, c(as.numeric(r3$cpu[2]), as.numeric(r4$cpu[2]),
                           r3$mlik[1] - r4$mlik[1], r3$misc$nfunc,
                           r4$misc$nfunc,
                           mean(abs(r3$mode$theta - r4$mode$theta)),
                           nfun/ii)))
}
print(nfun)
