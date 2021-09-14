library (INLA)
options(warn=1)
## Crossed Random Effects - Salamander

load("salam.RData")
## organize data into a form suitable for logistic regression
dat0=data.frame("y"=c(salam$y), "fW"=as.integer(salam$x[,"W/R"]==1 | salam$x[,"W/W"]==1), 
    "mW"=as.integer(salam$x[,"R/W"]==1 | salam$x[,"W/W"]==1), 
    "WW"=as.integer(salam$x[,"W/W"]==1 ) )
## add salamander id
id = t( apply(salam$z, 1, function(x) {
        tmp = which (x==1)
        tmp[2] = tmp[2] - 20
        tmp
    }) ) 
## ids are suitable for model A and C, but not B
id.modA = rbind(id, id+40, id+20)
colnames (id.modA) = c("f.modA","m.modA")
dat0=cbind (dat0, id.modA, group=1)
dat0$experiment=as.factor(rep(1:3, each=120))
dat0$group=as.factor(dat0$group)

salamander = dat0
salamander.e1 = subset (dat0, dat0$experiment==1)
salamander.e2 = subset (dat0, dat0$experiment==2)
salamander.e3 = subset (dat0, dat0$experiment==3)

corrfact<-1e6

# salamander.e1
salamander.e1.inla.fit = inla(y~fW+mW+WW + f(f.modA, model="iid", param=c(1,.622)) + f(m.modA, model="iid", param=c(1,.622)), 
        family="binomial", data=salamander.e1, Ntrials=rep(1,nrow(salamander.e1)))
salamander.e1.hyperpar = inla.hyperpar (salamander.e1.inla.fit)

salamander.e1.inla.fit.corrected = inla(y~fW+mW+WW + f(f.modA, model="iid", param=c(1,.622)) + f(m.modA, model="iid", param=c(1,.622)), 
    family="binomial", data=salamander.e1, Ntrials=rep(1,nrow(salamander.e1)),
                                        control.inla=list(correct=TRUE,correct.factor=corrfact))
salamander.e1.hyperpar.corrected = inla.hyperpar (salamander.e1.inla.fit.corrected)

## salamander.e2 
salamander.e2.inla.fit = inla(y~fW+mW+WW + f(f.modA, model="iid", param=c(1,.622)) + f(m.modA, model="iid", param=c(1,.622)), 
        family="binomial", data=salamander.e2, Ntrials=rep(1,nrow(salamander.e2)))
salamander.e2.hyperpar = inla.hyperpar (salamander.e2.inla.fit)

salamander.e2.inla.fit.corrected = inla(y~fW+mW+WW + f(f.modA, model="iid", param=c(1,.622)) + f(m.modA, model="iid", param=c(1,.622)), 
    family="binomial", data=salamander.e2, Ntrials=rep(1,nrow(salamander.e2)),
                                        control.inla=list(correct=TRUE,correct.factor=corrfact))
salamander.e2.hyperpar.corrected = inla.hyperpar (salamander.e2.inla.fit.corrected)

## salamander.e3
salamander.e3.inla.fit = inla(y~fW+mW+WW + f(f.modA, model="iid", param=c(1,.622)) + f(m.modA, model="iid", param=c(1,.622)), 
        family="binomial", data=salamander.e3, Ntrials=rep(1,nrow(salamander.e3)))
salamander.e3.hyperpar = inla.hyperpar (salamander.e3.inla.fit)

salamander.e3.inla.fit.corrected = inla(y~fW+mW+WW + f(f.modA, model="iid", param=c(1,.622)) + f(m.modA, model="iid", param=c(1,.622)), 
    family="binomial", data=salamander.e3, Ntrials=rep(1,nrow(salamander.e3)),
                                        control.inla=list(correct=TRUE,correct.factor=corrfact))
salamander.e3.hyperpar.corrected = inla.hyperpar (salamander.e3.inla.fit.corrected)


## Run JAGS:
library(runjags)
load.module("glm")

fid<-salamander.e1$f.modA
mid<-salamander.e1$m.modA
nMCMC<-1000
burnin<-100
m<-20
n<-120

# e1
jags1 <- run.jags(model = "salamFong.jags",
                  data = list(
                      m=m,
                      n=n,
                      y=salamander.e1$y,
                      fW=salamander.e1$fW,
                      mW=salamander.e1$mW,
                      WW=salamander.e1$WW,
                      fid=fid,
                      mid=mid),
                  monitor = c("beta","prec.bF","prec.bM"), 
                  method = 'parallel', 
                  burnin = burnin, 
                  n.chains = 10,
                  adapt = burnin,
                  sample = nMCMC)
mcmc1 = INLA:::inla.runjags2dataframe(jags1)

# e2
jags2 <- run.jags(model = "salamFong.jags",
                   data = list(
                       m=m,
                       n=n,
                       y=salamander.e2$y,
                       fW=salamander.e2$fW,
                       mW=salamander.e2$mW,
                       WW=salamander.e2$WW,
                       fid=fid,
                       mid=mid),
                  monitor = c("beta","prec.bF","prec.bM"), 
                  method = 'parallel', 
                  burnin = burnin, 
                  n.chains = 10,
                  adapt = burnin,
                  sample = nMCMC)
mcmc2 = INLA:::inla.runjags2dataframe(jags2)


# e3
jags3 <- run.jags(model = "salamFong.jags",
                   data = list(
                       m=m,
                       n=n,
                       y=salamander.e3$y,
                       fW=salamander.e3$fW,
                       mW=salamander.e3$mW,
                       WW=salamander.e3$WW,
                       fid=fid,
                       mid=mid),
                  monitor = c("beta","prec.bF","prec.bM"), 
                  method = 'parallel', 
                  burnin = burnin, 
                  n.chains = 10,
                  adapt = burnin,
                  sample = nMCMC)
mcmc3 = INLA:::inla.runjags2dataframe(jags3)

## collect results:
beta1<-(mcmc1$beta)
beta2<-(mcmc2$beta)
beta3<-(mcmc3$beta)
logprec.bF1<-log(mcmc1$prec.bF)
logprec.bF2<-log(mcmc2$prec.bF)
logprec.bF3<-log(mcmc3$prec.bF)
logprec.bM1<-log(mcmc1$prec.bM)
logprec.bM2<-log(mcmc2$prec.bM)
logprec.bM3<-log(mcmc3$prec.bM)


stop("changes needed from here on...")


## plot results:
pdf(paste("salamFong_correctFactor",corrfact,".pdf",sep=""))
betanames<-c("intercept","fW","mW","WW")
## exp1
for (i in 1:4)
{
    plot(inla.smarginal(salamander.e1.hyperpar$marginals.fixed[[i]]),type="l",lwd=2,xlab=betanames[i],ylab="density",
         main="Salamander experiment 1")
    legend("topleft",fill=c("black","red"),legend=c("Uncorrected","Corrected"))
    lines(inla.smarginal(salamander.e1.hyperpar.corrected$marginals.fixed[[i]]),lwd=2,col="red")
    hist(beta1[,i],prob=TRUE,nclass=100,add=TRUE)
}
plot(inla.smarginal(salamander.e1.hyperpar$internal.marginals.hyperpar[[1]]),type="l",lwd=2,
     xlab="bF hyperpar log precision",ylab="density",main="Salamander experiment 1")
legend("topleft",fill=c("black","red"),legend=c("Uncorrected","Corrected"))
lines(inla.smarginal(salamander.e1.hyperpar.corrected$internal.marginals.hyperpar[[1]]),lwd=2,col="red")
hist(logprec.bF1,prob=TRUE,nclass=100,add=TRUE)
plot(inla.smarginal(salamander.e1.hyperpar$internal.marginals.hyperpar[[2]]),type="l",lwd=2,
     xlab="bM hyperpar log precision",ylab="density",main="Salamander experiment 1")
legend("topleft",fill=c("black","red"),legend=c("Uncorrected","Corrected"))
lines(inla.smarginal(salamander.e1.hyperpar.corrected$internal.marginals.hyperpar[[2]]),lwd=2,col="red")
hist(logprec.bM1,prob=TRUE,nclass=100,add=TRUE)

## exp2
for (i in 1:4)
    {
        plot(inla.smarginal(salamander.e2.hyperpar$marginals.fixed[[i]]),type="l",lwd=2,xlab=betanames[i],ylab="density",
             main="Salamander experiment 1")
        legend("topleft",fill=c("black","red"),legend=c("Uncorrected","Corrected"))
        lines(inla.smarginal(salamander.e2.hyperpar.corrected$marginals.fixed[[i]]),lwd=2,col="red")
        hist(beta2[,i],prob=TRUE,nclass=100,add=TRUE)
    }
plot(inla.smarginal(salamander.e2.hyperpar$internal.marginals.hyperpar[[1]]),type="l",lwd=2,
     xlab="bF hyperpar log precision",ylab="density",main="Salamander experiment 1")
legend("topleft",fill=c("black","red"),legend=c("Uncorrected","Corrected"))
lines(inla.smarginal(salamander.e2.hyperpar.corrected$internal.marginals.hyperpar[[1]]),lwd=2,col="red")
hist(logprec.bF2,prob=TRUE,nclass=100,add=TRUE)
plot(inla.smarginal(salamander.e2.hyperpar$internal.marginals.hyperpar[[2]]),type="l",lwd=2,
     xlab="bM hyperpar log precision",ylab="density",main="Salamander experiment 1")
legend("topleft",fill=c("black","red"),legend=c("Uncorrected","Corrected"))
lines(inla.smarginal(salamander.e2.hyperpar.corrected$internal.marginals.hyperpar[[2]]),lwd=2,col="red")
hist(logprec.bM2,prob=TRUE,nclass=100,add=TRUE)

## exp3
for (i in 1:4)
    {
        plot(inla.smarginal(salamander.e3.hyperpar$marginals.fixed[[i]]),type="l",lwd=2,xlab=betanames[i],ylab="density",
             main="Salamander experiment 1")
        legend("topleft",fill=c("black","red"),legend=c("Uncorrected","Corrected"))
        lines(inla.smarginal(salamander.e3.hyperpar.corrected$marginals.fixed[[i]]),lwd=2,col="red")
        hist(beta3[,i],prob=TRUE,nclass=100,add=TRUE)
    }
plot(inla.smarginal(salamander.e3.hyperpar$internal.marginals.hyperpar[[1]]),type="l",lwd=2,
     xlab="bF hyperpar log precision",ylab="density",main="Salamander experiment 3")
legend("topleft",fill=c("black","red"),legend=c("Uncorrected","Corrected"))
lines(inla.smarginal(salamander.e3.hyperpar.corrected$internal.marginals.hyperpar[[1]]),lwd=2,col="red")
hist(logprec.bF3,prob=TRUE,nclass=100,add=TRUE)
plot(inla.smarginal(salamander.e3.hyperpar$internal.marginals.hyperpar[[2]]),type="l",lwd=2,
     xlab="bM hyperpar log precision",ylab="density",main="Salamander experiment 3")
legend("topleft",fill=c("black","red"),legend=c("Uncorrected","Corrected"))
lines(inla.smarginal(salamander.e3.hyperpar.corrected$internal.marginals.hyperpar[[2]]),lwd=2,col="red")
hist(logprec.bM3,prob=TRUE,nclass=100,add=TRUE)

dev.off()
