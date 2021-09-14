## Function implementing copula correction
## Input: INLA object
## Output: Two different corrections, without and with skewness

correct <- function(inlaobj,doplot=FALSE,mcmcres=NULL,title="")
{
    cc = inlaobj$misc$configs
    nc = cc$nconfig
    ## only include fixed effects:
    latentFieldLenghts <- rev(inlaobj$misc$configs$contents$length)
    nFixedEffects <- min(which(latentFieldLenghts>1))-1
    totLength <- sum(latentFieldLenghts)
    out <- 1:(totLength-nFixedEffects)
    theta = numeric(nc)
    ldens = numeric(nc)
    correctionTerm1 = numeric(nc)
    correctionTerm2 = numeric(nc)
    for(idx in 1:nc) {
        theta[idx] = cc$config[[idx]]$theta
        ldens[idx] = cc$config[[idx]]$log.posterior
        m = cc$config[[idx]]$mean
        v = diag(cc$config[[idx]]$Qinv)
        sigma <- sqrt(v)
        mm = cc$config[[idx]]$improved.mean
        skew = cc$config[[idx]]$skewness
        sn.param = matrix(NA, length(m), 3)
        for(k in 1:length(m)) {
            sn.param[k, ] = INLA:::inla.sn.reparam(moments = c(mm[k], v[k], skew[k]))
        }
        delta.m = as.matrix(m - mm, ncol = 1)
        Q = cc$config[[idx]]$Q
        d = diag(Q)
        diag(Q) = 0
        Q = Q + t(Q)
        diag(Q) = d
        S.fixed = solve(Q)[-out, -out]
        Q.fixed = solve(S.fixed)
        delta.m = delta.m[-out, 1]
        ## simple correction term without skewness:
        correctionTerm1[idx] <- (0.5 * t(delta.m) %*% Q.fixed %*% delta.m)[1, 1]
        ## complicated correction term with skewness:
        correctionTerm2[idx] <- 0
        for(k in (max(out)+1):length(m)) {
            for (l in (max(out)+1):length(m)) {
                correctionTerm2[idx] <- correctionTerm2[idx] +
                    0.5*Q.fixed[k-max(out),l-max(out)]*sigma[k]*sigma[l]*
                        qnorm(psn(m[k],xi=sn.param[k,1],omega=sn.param[k,2],alpha=sn.param[k,3]))*
                            qnorm(psn(m[l],xi=sn.param[l,1],omega=sn.param[l,2],alpha=sn.param[l,3]))
            }
            ## add the Jacobian for the complicated correction term:
            correctionTerm2[idx] <- correctionTerm2[idx] +
                dnorm(qnorm(psn(m[k],xi=sn.param[k,1],omega=sn.param[k,2],alpha=sn.param[k,3])),log=TRUE)-
                    dsn(m[k],xi=sn.param[k,1],omega=sn.param[k,2],alpha=sn.param[k,3],log=TRUE)-log(sigma[k])
        }
    }
    ldensCorr1 <- ldens + correctionTerm1
    ldensCorr2 <- ldens + correctionTerm2
    ldens <- ldens - max(ldens)
    ldensCorr1 <- ldensCorr1 - max(ldensCorr1)
    ldensCorr2 <- ldensCorr2 - max(ldensCorr2)

    ## get means and variances for fixed effects:
    fixedIdx <- (1:totLength)[-out]
    nFixed <- length(fixedIdx)
    meanFixedOrig <- numeric(nFixed)    ## just a check
    varFixedOrig <- numeric(nFixed)     ## just a check
    meanFixedCorr1 <- numeric(nFixed)
    varFixedCorr1 <- numeric(nFixed)
    meanFixedCorr2 <- numeric(nFixed)
    varFixedCorr2 <- numeric(nFixed)
    m <- matrix(nrow=nc,ncol=nFixed)
    v <- matrix(nrow=nc,ncol=nFixed)
    for (i in 1:nc)
    {
        for (j in 1:nFixed)
        {
            m[i,j] <- cc$config[[i]]$improved.mean[fixedIdx[j]]
            v[i,j] <- cc$config[[i]]$Qinv[fixedIdx[j],fixedIdx[j]]
        }
    }
    p.Orig <- exp(ldens)/sum(exp(ldens))
    p.Corr1 <- exp(ldensCorr1)/sum(exp(ldensCorr1))
    p.Corr2 <- exp(ldensCorr2)/sum(exp(ldensCorr2))
    for (j in 1:nFixed)
    {
        meanFixedOrig[j] <- sum(p.Orig*m[,j])
        varFixedOrig[j] <- sum(p.Orig*(m[,j]^2 + v[,j])) - meanFixedOrig[j]^2
        meanFixedCorr1[j] <- sum(p.Corr1*m[,j])
        varFixedCorr1[j] <- sum(p.Corr1*(m[,j]^2 + v[,j])) - meanFixedCorr1[j]^2
        meanFixedCorr2[j] <- sum(p.Corr2*m[,j])
        varFixedCorr2[j] <- sum(p.Corr2*(m[,j]^2 + v[,j])) - meanFixedCorr2[j]^2
    }
    sdFixedOrig<-sqrt(varFixedOrig)
    sdFixedCorr1<-sqrt(varFixedCorr1)
    sdFixedCorr2<-sqrt(varFixedCorr2)
        
        
    ## sort everything by increasing theta (for easier plotting):
    ## o <- order(theta)
    ## theta <- theta[o]
    ## ldens <- ldens[o]
    ##  correctionTerm1 <- correctionTerm1[o]
    ## correctionTerm2 <- correctionTerm2[o]
    ## ldensCorr1 <- ldensCorr1[o]
    ## ldensCorr2 <- ldensCorr2[o]
        

        
    ## if (doplot)
    ##     {
    ##         thetaRange <- diff(range(theta))
    ##         xMin <- min(theta)-0.2*thetaRange
    ##         xMin <- min(log(mcmcres),xMin)
    ##         xMax <- max(theta)+0.2*thetaRange
    ##         xMax <- max(log(mcmcres),xMax)
    ##         yMin <- 0
    ##         yMax <- 1.5*max(inla.smarginal(inlaobj$internal.marginals.hyperpar[[1]])$y)
    ##         xx <- seq(xMin, xMax, len=10000)
    ##         m1 <- list(x = theta,  y = exp(ldensCorr1))
    ##         sm1 <- inla.smarginal(m1)
    ##         dsum1 <- sum(inla.dmarginal(xx, sm1) * diff(xx)[1])
    ##         m2 <- list(x = theta,  y = exp(ldensCorr2))
    ##         sm2 <- inla.smarginal(m2)
    ##         dsum2 <- sum(inla.dmarginal(xx, sm2) * diff(xx)[1])
    ##         if (!is.null(mcmcres))
    ##             {
    ##                 hist(log(mcmcres), xlab="tau.b1", prob=TRUE, n=40,ylab="",xlim=c(xMin,xMax),ylim=c(yMin,yMax),main=title)
    ##                 lines(inla.smarginal(inlaobj$internal.marginals.hyperpar[[1]]), col="blue") 
    ##             } else plot(inla.smarginal(inlaobj$internal.marginals.hyperpar[[1]],),
    ##                         main=title, col="blue", type="l", ylim=c(yMin,yMax),xlab="",ylab="")
    ##         lines(xx, inla.dmarginal(xx, sm1)/dsum1, col="red", lwd=1)
    ##         lines(xx, inla.dmarginal(xx, sm2)/dsum2, col="green", lwd=1)
    ##     }
    list(ldens=ldens,
         theta=theta,
         correctionTerm1=correctionTerm1,
         correctionTerm2=correctionTerm2,
         ldensCorr1=ldensCorr1,
         ldensCorr2=ldensCorr2,
         meanFixedOrig=meanFixedOrig,
         meanFixedCorr1=meanFixedCorr1,
         meanFixedCorr2=meanFixedCorr2,
         varFixedOrig=varFixedOrig,
         varFixedCorr1=varFixedCorr1,
         varFixedCorr2=varFixedCorr2,
         sdFixedOrig=sdFixedOrig,
         sdFixedCorr1=sdFixedCorr1,
         sdFixedCorr2=sdFixedCorr2)
        
        
}

plotCorrTerm <- function(corres,title)
    {
        plot(corres$theta,corres$correctionTerm1-max(corres$correctionTerm1),
             type="l",col="red",main=title,xlab="theta",ylab="correction")
        lines(corres$theta,corres$correctionTerm2-max(corres$correctionTerm1),col="green")
    }
