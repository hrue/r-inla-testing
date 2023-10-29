library(INLA) # Bayesian inference with INLA
library(INLAjoint) # Interface for joint modeling with INLA
library(PermAlgo) # Permutation algorithm to generate survival times dependent on time-varying covariates
library(mvtnorm)
inla.setOption(smtp="taucs")
set.seed(1)

# data simulation
nsujet=500 # number of indivduals
nmarkers=10 # number of longitudinal markers

# longitudinal markers fixed effects
b_0=0.2 # intercept
b_1=-0.1 # slope
b_12=0.1 # quadratic slope
b_2=0.1 # continuous covariate
b_3=-0.2 # binary covariate
b_e=sqrt(0.16) # residual error


# Survival
b_cv=0.5 # effect of markers linear predictors on the risk of event
b_cs=-0.2 # effect of markers current slope on the risk of event

gapLongi=0.5 # gap between longi measurements
gap=0.01 # used to generate a lot of time points because the permutation
# algorithm chooses among those time points to define survival times
followup=5 # follow-up time
mestime=seq(0,followup,gap) # measurement times
timesLongi=mestime[which(round(mestime-round(mestime/gapLongi,0)*gapLongi,6)==0)] # visit times
time=rep(mestime, nsujet) # time column
nmesindiv=followup/gap+1 # max. number of individual measurements
nmesy= nmesindiv*nsujet # max. total number of longi measurements
# the number is reduced because of censoring by the terminal event
idY<-rep(1:nsujet, each=nmesindiv) # individual id

# random effects variance and covariance matrix
Sigma <- matrix(c(0.16, 0.03, 0.02,
                  0.03, 0.25, 0.03,
                  0.02, 0.03, 0.09),ncol=3,nrow=3)
ctsX=rep(rnorm(nsujet,1, 0.5) , each=nmesindiv) # continuous covariate
binX=rep(rbinom(nsujet,1, 0.5), each=nmesindiv) # binary covariate

linPredY <- vector("list", nmarkers)
slopeY <- vector("list", nmarkers)
Y <- vector("list", nmarkers)

for(i in 1:nmarkers){
  MVnorm <- rmvnorm(nsujet, rep(0, 3), Sigma)
  b_int <- rep(MVnorm[,1], each=nmesindiv) # random intercept Y1
  b_slo <- rep(MVnorm[,2], each=nmesindiv) # random slope Y1
  b_slo2 <- rep(MVnorm[,3], each=nmesindiv) # random slope Y1
  linPredY[[i]] <- (b_0+b_int) + (b_1+b_slo)*time + (b_12+b_slo2)*time^2 + b_2*ctsX + b_3*binX
  slopeY[[i]] <- b_1+b_slo + 2*(b_12+b_slo2)*time
  Y[[i]] <- rnorm(nmesy, linPredY[[i]], b_e)
}
Yobs <- matrix(c(unlist(Y)), nrow=nsujet*nmesindiv)
colnames(Yobs) <- paste0("Y", 1:nmarkers)

Xmat <- matrix(c(unlist(linPredY), unlist(slopeY)), nrow=nsujet*nmesindiv)
XmatNames <- c(paste0("linPredY", 1:nmarkers), paste0("slopeY", 1:nmarkers))
betas <- c(rep(b_cv, nmarkers), rep(b_cs, nmarkers))
# Permutation algorithm to generate survival times that depends on the linear predictors
DatTmp <- permalgorithm(nsujet,nmesindiv,Xmat=Xmat,
                        eventRandom = round(rexp(nsujet, 0.001)+1,0), # ~40% death
                        censorRandom=runif(nsujet,1,nmesindiv), # uniform random censoring
                        XmatNames=XmatNames, # association
                        betas=betas) # association parameters

# extract last line for each Id (= death/censoring time)
DatTmp2=DatTmp[c(which(diff(DatTmp[,"Id"])==1), dim(DatTmp)[1]), c("Id","Event","Stop")]
DatTmp2$deathTimes <- mestime[DatTmp2$Stop+1] # deathtimes
survDat <- DatTmp2[, c("Id", "deathTimes", "Event")]
DatTmp$time <- mestime[DatTmp$Start+1] # measurements times of the biomarker
DatTmp$Uid <- paste(DatTmp$Id, DatTmp$time) # unique identifier to match covariates and observed biomarker values
longDat3 <- merge(DatTmp[,c("Uid", "Id", "time")], cbind("Uid"=paste(idY,time), ctsX, binX, Yobs), by=c("Uid"))
longDat <- sapply(longDat3[longDat3$time%in%timesLongi,-1], as.numeric)
longDat <- as.data.frame(longDat[order(longDat[, "Id"], longDat[, "time"]),])
print(summary(survDat)) # survival dataset
print(summary(longDat)) # longitudinal dataset
print(str(survDat))
print(str(longDat))


# Model fit with INLAjoint
f1 <- function(x) x^2
formLong <- sapply(paste0("Y", 1:nmarkers, "~ time + f1(time) + ctsX + binX + (1 + time + f1(time)|Id)"), as.formula)
SIM_large2 <- joint(formLong = formLong,
                formSurv = inla.surv(time = deathTimes, event = Event) ~ -1,
                dataSurv = survDat, dataLong = longDat,
                basRisk = "rw2", id = "Id", corLong=F, timeVar = "time",
                family = rep("gaussian", nmarkers),
                assoc = c(rep("CV_CS", nmarkers)), control=list(int.strategy="eb", verbose=T))


class(SIM_large2) <- "inla"
summary(SIM_large2)




