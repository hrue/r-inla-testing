library(INLA)
library(R2jags)
load.module("glm")

load("toenail")
toenail$patientID = as.numeric(as.character(toenail$patientID))
toenail$treatment = as.numeric(toenail$treatment) -1 ## 0 or 1
toenail$outcome = as.numeric(toenail$outcome)-1

### run JAGS:

#5.3.1 RI with Gamma prior
                                        #setup data and parameters.

#MCMC iterations, burnin, thinning:
niter <- 60000
burnin <- 10000
thin <- 1
d <- 1
q <- 0.95
R <- log(10)
a1 <- d/2
a2 <- (R^2*d)/(2*(qt((1-(1-q)/2), df=d))^2)
##a1<-a2<-.01
y<-toenail$outcome
treatment<-toenail$treatment
time<-toenail$time ##-mean(toenail$time)
timetrt<-time*treatment
n<-nrow(toenail)
m<-length(unique(toenail$patientID))
ID<-as.numeric(as.factor(toenail$patientID))
## jags.data <- list("y","treatment", "time", "timetrt", "n","m", "ID", "a1", "a2")
## jags.params <- c("beta", "tau.b")
## ## cat("RI, JAGS...\n")

##                                         #setup JAGS-code
## cat("## model Random intercept:
##     model{
##       for(i in 1:n){
## 	y[i]~dbern(p[i])
## 	logit(p[i]) <-  beta[1]+ beta[2]*treatment[i]+beta[3]*time[i]+
## 			beta[4]*timetrt[i]+b[ID[i]]
##       }
##       for(i in 1:4){
##       beta[i]~dnorm(0, 0.0001)
##       }
##       for(j in 1:m){
## 	b[j]~dnorm(0,tau.b)
##       }
##       tau.b~dgamma(a1,a2)
##     }",fill=T,file="jags1i.txt")

##                                         #Call and transform JAGS.
## jags1i <- jags(data=jags.data, n.iter=niter,n.burnin=0, n.thin=thin,n.chains=1, 
##                parameters.to.save=jags.params,model.file="jags1i.txt")
## jags1imcmc <- as.mcmc(jags1i)[[1]][(burnin+1):niter,-5]
## save(jags1imcmc,file="jags1imcmc_240215.RData")
load("jags1imcmc_uncentered.RData")


## INLA:



inladat<-data.frame(response=y,treatment=treatment,time=time,timetrt=timetrt,ID=ID)

f1i <- response~treatment+time+timetrt+f(ID, model="iid", param=c(a1, a2))

corrfact<-25

cat("\ncorrect.factor =",corrfact,"\n")
cat("RI with Gamma prior...\n")
inla1i<- inla(f1i, family="binomial", 
              data=inladat,
              control.inla=list(int.strategy="grid",
                  diff.logdens = 20,  dz = 0.2),
              control.compute = list(config=TRUE),
              verbose=F,
              control.fixed=list(mean.intercept=0, mean=0, prec=0.0001, 
                  prec.intercept=0.0001))


cat("RI with Gamma prior, corrected...\n")
inla1ic <-  inla(f1i, family="binomial", 
                 data=inladat,
                 control.inla=list(int.strategy="grid",
                     diff.logdens = 20,  dz = 0.2,
                     correct=TRUE,
                     correct.factor=corrfact,
                                   correct.verbose=FALSE),
                 verbose=TRUE,
                 control.fixed=list(mean.intercept=0, mean=0, prec=0.0001, 
                     prec.intercept=0.0001))



cc = inla1i$misc$configs
nc = cc$nconfig

theta = numeric(nc)
ldens = numeric(nc)
ldensCorr = numeric(nc)
correct = numeric(nc)

for(idx in 1:nc) {
    theta[idx] = cc$config[[idx]]$theta
    ldens[idx] = cc$config[[idx]]$log.posterior
    m = cc$config[[idx]]$mean
    v = diag(cc$config[[idx]]$Qinv)
    mm = cc$config[[idx]]$improved.mean
    delta.m = as.matrix(m - mm, ncol = 1)
    Q = cc$config[[idx]]$Q
    d = diag(Q)
    diag(Q) = 0
    Q = Q + t(Q)
    diag(Q) = d
    ## do not include the linear predictor...
    out = 1:2202
    S = solve(Q)[-out, -out]
    ##print(S)
    ##S = diag(v[-out])
    delta.m = delta.m[-out, 1]
    correct[idx] = (0.5 * t(delta.m) %*% solve(S) %*% delta.m)[1, 1]
    ldensCorr[idx] = ldens[idx] + correct[idx]

}
    
        ## get means and variances for fixed effects:
        
nFixed <- 4
meanFixedOrig <- numeric(nFixed) ## just a check
varFixedOrig <- numeric(nFixed) ## just a check
meanFixedCorr <- numeric(nFixed)
varFixedCorr <- numeric(nFixed)
        
m <- matrix(nrow=nc,ncol=nFixed)
v <- matrix(nrow=nc,ncol=nFixed)
fixedIdx<-2203:2206
for (i in 1:nc)
    {
        for (j in 1:nFixed)
            {
                m[i,j] <- cc$config[[i]]$improved.mean[fixedIdx[j]]
                v[i,j] <- cc$config[[i]]$Qinv[fixedIdx[j],fixedIdx[j]]
            }
    }
p.Orig <- exp(ldens)/sum(exp(ldens))
p.Corr <- exp(ldensCorr)/sum(exp(ldensCorr))

for (j in 1:nFixed)
    {
        meanFixedOrig[j] <- sum(p.Orig*m[,j])
        varFixedOrig[j] <- sum(p.Orig*(m[,j]^2 + v[,j])) - meanFixedOrig[j]^2
        meanFixedCorr[j] <- sum(p.Corr*m[,j])
        varFixedCorr[j] <- sum(p.Corr*(m[,j]^2 + v[,j])) - meanFixedCorr[j]^2
    }
sdFixedOrig<-sqrt(varFixedOrig)
sdFixedCorr<-sqrt(varFixedCorr)


o<-order(theta)
theta<-theta[o]
ldensCorr<-ldensCorr[o]
xdiff<-diff(theta)[1]
densCorr<-exp(ldensCorr)/(sum(xdiff*exp(ldensCorr)))

xs <- seq(-6,6,by=.001)
parnames<-c(" intercept"," treatment"," time"," time X treatment")
pdf(paste("toenail_corrfact_",corrfact,".pdf",sep=""))
plot(inla.smarginal(inla1i$marginals.fixed[[1]]), type="l", lwd=2,
     main = "",xlab=expression(paste(beta," intercept")),ylab="posterior density")
lines(inla.smarginal(inla1ic$marginals.fixed[[1]]), lwd=2, col="green")
##lines(xs,dnorm(xs,mean=meanFixedCorr[1],sd=sdFixedCorr[1]), type="l", lwd=2,col="red", lty=2)
hist(jags1imcmc[,1],nclass=100,prob=TRUE,add=TRUE)
legend("topleft",fill=c("black","green"),legend=c("Uncorrected","Corrected (INLA)"))
plot(inla.smarginal(inla1i$marginals.fixed[[2]]), type="l", lwd=2,
     main = "",xlab=expression(paste(beta," treatment")),ylab="posterior density")
lines(inla.smarginal(inla1ic$marginals.fixed[[2]]), lwd=2, col="green")
##lines(xs,dnorm(xs,mean=meanFixedCorr[2],sd=sdFixedCorr[2]), type="l", lwd=2,col="red", lty=2)
hist(jags1imcmc[,2],nclass=100,prob=TRUE,add=TRUE)
legend("topleft",fill=c("black","green"),legend=c("Uncorrected","Corrected (INLA)"))

plot(inla.smarginal(inla1i$marginals.fixed[[3]]), type="l", lwd=2,
     main = "",xlab=expression(paste(beta," time")),ylab="posterior density")
lines(inla.smarginal(inla1ic$marginals.fixed[[3]]), lwd=2, col="green")
##lines(xs,dnorm(xs,mean=meanFixedCorr[3],sd=sdFixedCorr[3]), type="l", lwd=2,col="red", lty=2)
hist(jags1imcmc[,3],nclass=100,prob=TRUE,add=TRUE)
legend("topleft",fill=c("black","green"),legend=c("Uncorrected","Corrected (INLA)"))

plot(inla.smarginal(inla1i$marginals.fixed[[4]]), type="l", lwd=2,
     main = "",xlab=expression(paste(beta," time X treatment")),ylab="posterior density")
lines(inla.smarginal(inla1ic$marginals.fixed[[4]]), lwd=2, col="green")
##lines(xs,dnorm(xs,mean=meanFixedCorr[4],sd=sdFixedCorr[4]), type="l", lwd=2,col="red", lty=2)
hist(jags1imcmc[,4],nclass=100,prob=TRUE,add=TRUE)
legend("topleft",fill=c("black","green"),legend=c("Uncorrected","Corrected (INLA)"))

plot(inla.smarginal(inla1i$internal.marginals.hyperpar[[1]]), type="l", lwd=2,
     main = "",xlab="log precision of random effect",ylab="posterior density")
lines(inla.smarginal(inla1ic$internal.marginals.hyperpar[[1]]), lwd=2, col="green")
hist(log(jags1imcmc[,5]),nclass=100,prob=TRUE,add=TRUE)
legend("topleft",fill=c("black","green"),legend=c("Uncorrected","Corrected (INLA)"))

dev.off()
    

