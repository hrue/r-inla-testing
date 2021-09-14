## Check correction on simple example...
library(INLA)
library(sn)
source("correct.R")
inla.setOption(num.threads=1)
inla.setOption(inla.call="inla.work")

set.seed(123986123)
nsim<-1
sigma0 <- 1
ni <- 100
nj <- 7
eta <- array(dim=c(ni,nj,nsim))
y <- array(dim=c(ni,nj,nsim))
beta0 <- -2.5
beta1 <- 1.0
beta2 <- -1.0
beta3 <- -0.5
tt <- (-3):3
xx <- c(rep(0,ni/2),rep(1,ni/2))
b0 <- matrix(rnorm(ni*nsim,mean=0,sd=sigma0),nrow=ni,ncol=nsim)


k<-1

for (i in 1:ni) for (j in 1:nj) for (k in 1:nsim) eta[i,j,k] <- beta0 + beta1*tt[j] + beta2*xx[i] + beta3*tt[j]*xx[i] + b0[i,k]   
p <- 1/(1+exp(-eta))
for (i in 1:ni) for (j in 1:nj) for (k in 1:nsim) y[i,j,k] <- rbinom(n=1, size=1, prob=p[i,j,k])

inladata <- list()
ind <- rep(1:ni,nj)

        y.inla<-as.vector(y[,,k])
        x.inla<-rep(xx,nj)
        t.inla<-rep(tt,each=ni)
        inladata[[k]]<-data.frame(y=y.inla, x=x.inla, t=t.inla)

inlaformula <- y~t*x + f(ind, model="iid", hyper=list(theta=list(prior="loggamma",param=c(0.5, 0.0164))))

corrfact<-1000

cat("Running INLA...\n")
inlares<-inla(inlaformula,data=inladata[[k]],family="binomial",Ntrials=rep(1,ni*nj),
                       control.inla = list(int.strategy = "grid",
                           diff.logdens = 10,
                           dz = 0.25,
                           force.diagonal=TRUE, 
                           interpolator = "gridsum"), 
              control.fixed = list(prec.intercept = 1E-4, prec = 1E-4, correlation.matrix=TRUE),
              control.predictor = list(compute=TRUE),
              control.compute = list(config=TRUE),
                       verbose=FALSE)
write(inlares$logfile,  file="logfile")
system("./extract")
nocorrectSL = read.table("correct.dat")

cat("Running INLA with SL correction...\n")
inlacorrectedSL<-inla(inlaformula,data=inladata[[k]],family="binomial",Ntrials=rep(1,ni*nj),
                       control.inla = list(int.strategy = "grid",
                           diff.logdens = 10,
                           dz = 0.25,
                           force.diagonal=TRUE, 
                           interpolator = "gridsum",
                           correct = TRUE, correction.factor = corrfact, correction.strategy="simplified.laplace"),
                       control.fixed = list(prec.intercept = 1E-4, prec = 1E-4),
                            control.predictor = list(compute=TRUE),
                            control.mode = list(result = inlares, restart = TRUE),
                      verbose=FALSE)
write(inlacorrectedSL$logfile,  file="logfile")
system("./extract")
correctSL = read.table("correct.dat")


dev.new()
## see if I can reconstruct the pmarg using the correction
cfunc = splinefun(correctSL$V1,  correctSL$V3)
m = inla.smarginal(inlares$internal.marginals.hyperpar[[1]])
m$y = exp(log(m$y) + cfunc(m$x))
dx = diff(range(m$x))/10000
m$y = m$y / sum(dx * inla.dmarginal(seq(range(m$x)[1], range(m$x)[2], by = dx), m))
plot(m,  type = "l", lty=2, lwd=2, col="blue", main="marginal computed by adding correction and
comparing with inlacorrectedSL")
lines(inla.smarginal(inlacorrectedSL$internal.marginals.hyperpar[[1]]), col="red", lwd=3, lty=2)


##stop("xxx")

if (FALSE) {
    cat("Running INLA with LA correction...\n")
    inlacorrectedLA<-inla(inlaformula,data=inladata[[k]],family="binomial",Ntrials=rep(1,ni*nj),
                          control.inla = list(int.strategy = "grid",
                              diff.logdens = 10,
                              dz = 0.25,
                              force.diagonal=TRUE, 
                              interpolator = "gridsum",
                              correct = TRUE, correction.factor = corrfact, correction.strategy="laplace"),
                          control.fixed = list(prec.intercept = 1E-4, prec = 1E-4),
                          control.predictor = list(compute=TRUE),
                          control.mode = list(result = inlares, restart = TRUE),
                          verbose=FALSE)
}

cat("Running manual correction...\n")
oldCorrection<-correct(inlares)

dev.new()
plot(oldCorrection$theta, oldCorrection$correctionTerm1, main="check correction term")
lines(correctSL[, 1], correctSL[, 3])

dev.new()
theta = oldCorrection$theta
ldens = oldCorrection$ldens
ldens = ldens - max(ldens)
o = order(theta)
theta = theta[o]
ldens = ldens[o]
plot(inla.smarginal(list(x = theta, y=exp(ldens))), type="l",
     main = "check uncorrected posterior", lwd=2, col="blue")
m =inla.smarginal(inlares$internal.marginals.hyperpar[[1]])
m$y = m$y / max(m$y)
lines(m, lwd=3, lty=2, col="red")

dev.new()
## compute corrected one
mm = inla.smarginal(list(x = theta, y = exp(oldCorrection$ldensCorr1[o])))
plot(mm, type="l",  lwd=2,  col="blue",  main="computed manually")
## add the SL one
m = inla.smarginal(inlacorrectedSL$internal.marginals.hyperpar[[1]])
m$y = m$y/max(m$y)
lines(m,  col="red", lwd=3, lty=2)

