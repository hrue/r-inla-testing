
set.seed(1)
DR <- FALSE

PACK <- "INLA" # Select package ("INLA" or "frailtypack", put anything else to simulate the dataset without estimation)
nsim=1 # number of datasets for simulations
CPID <- 0 # used for parallel computing with frailtypack

# load packages
if(PACK=="INLA"){
    suppressMessages(library(INLA))
    if(DR) inla.setOption(pardiso.license = "/users/dr/pardiso.lic")
    INLA_res <- matrix(ncol=37, nrow=nsim) # matrix with results for each model
} else if(PACK=="frailtypack") {
    suppressMessages(library(frailtypack))
    suppressMessages(library(Rmpi)) # MPI parallelization
    CPID <- mpi.comm.rank(0)
    FP_res <-  matrix(ncol=41, nrow=nsim) # matrix with results for each model
    maxITER=100
    seed_MC=1 # Monte-carlo seed
}
suppressMessages(library(mvtnorm)) # for multivariate normal generation (random-effects)
nsujet=10000 # number of individuals

#binary part  
alpha_0=4 # Intercept
alpha_1=-1 # slope
alpha_2=-1 # treatment
alpha_3=1 # treatment x time

#continuous part
beta_0=2 # Intercept
beta_1=-0.3 # slope
beta_2=-0.3 # treatment
beta_3=0.3 # treatment x time
sigma_e=0.4 # error term (standard error)

gamma_1=0.2 # trt effect on survival

# association
phi_a=0.5 # random intercept (binary)
phi_b=0.5 # random intercept (continuous)
phi_bt=0.5 # random slope (continuous)

baseScale=0.4 # baseline hazard scale (to generate exponential death times)

gap=0.4 # gap between longi repeated measurements

followup=4 # study duration

sigma_a=1 # random intercept (binary)
sigma_b=0.5 # random intercept (continuous)
sigma_bt=0.5 # random slope (continuous)
cor_ba=0.5 # correlation intercept (binary)/intercept (continuous)
cor_bta=0.5 # correlation intercept (binary)/slope (continuous)
cor_bbt=-0.2 # correlation continuous intercept/slope

# covariance
cov_ba <- sigma_b*sigma_a*cor_ba
cov_bta <- sigma_bt*sigma_a*cor_bta
cov_bbt <- sigma_b*sigma_bt*cor_bbt

# variance-covariance matrix
Sigma=matrix(c(sigma_a^2,cov_ba,cov_bta,
               cov_ba,sigma_b^2,cov_bbt,
               cov_bta,cov_bbt,sigma_bt^2),ncol=3,nrow=3)

mestime=seq(0,followup,gap) # measurement times
timej=rep(mestime, nsujet) # time column 
nmesindiv=followup/gap+1 # number of individual measurements
nmesy= nmesindiv*nsujet # number of longi measurements
id<-as.factor(rep(1:nsujet, each=nmesindiv)) # patient id


MVnorm <- mvtnorm::rmvnorm(nsujet, rep(0, 3), Sigma)
a_i = MVnorm[,1] # binary intercept
a_iY <- rep(a_i, each=nmesindiv) # binary intercept (repeated for longi dataset)
b_i = MVnorm[,2] # continuous intercept
b_iY <- rep(b_i, each=nmesindiv)
bt_i = MVnorm[,3] # continuous slope
bt_iY <- rep(bt_i, each=nmesindiv)

trt=rbinom(nsujet,1, 0.5) # treatment covariate
trtY=rep(trt, each=nmesindiv)

## linear predictor (binary part)
linPredBin <- alpha_0+a_iY+alpha_1*timej+alpha_2*trtY+alpha_3*timej*trtY
probaBin <- exp(linPredBin)/(1+exp(linPredBin)) # proba of positive value
B <- rbinom(nmesy,1, probaBin) # observed zero values

## linear predictor (continuous part)
linPredCont <- beta_0+b_iY+(beta_1+bt_iY)*timej+beta_2*trtY+beta_3*timej*trtY
mu=linPredCont-sigma_e^2/2 # lognormal mean
Ypos <- rlnorm(length(mu), meanlog = mu, sdlog = sigma_e) # observed biomarker values
Y = (ifelse(B==1, Ypos, 0)) # include zeros in the biomarker distribution

## longitudinal biomarker dataset
longDat <- data.frame(id, timej, trtY, Y)

## generation of exponential death times
u <- runif(nsujet) # uniform distribution for survival times generation
deathTimes <- -log(u) / (baseScale * exp(trt * gamma_1 + a_i*phi_a + b_i*phi_b + bt_i*phi_bt))

d <- as.numeric(deathTimes<followup) # deathtimes indicator

## censoring individuals at end of follow-up (not at random)
deathTimes[deathTimes>=followup]=followup 
id <- as.factor(1:nsujet)
survDat <- data.frame(id,deathTimes, d, trt) # survival times dataset

## removing longi measurements after death
ind <- rep(NA, nsujet*length(mestime)) 
for (i in 1:nsujet){ 
    for(j in 1:length(mestime)){
        if(longDat[(i-1)*length(mestime)+j, "timej"]<=survDat[i,"deathTimes"]) ind[(i-1)*length(mestime)+j]=1
    }
}
longDat <- longDat[!is.na(ind),]

survDat$trt <- as.factor(survDat$trt)
longDat$trtY <- as.factor(longDat$trtY)
longDat$id <- as.integer(longDat$id)
survDat$id <- as.integer(survDat$id)

## Summary of the longitudinal and survival datasets
if(CPID==0) print(summary(survDat))
if(CPID==0) print(summary(longDat))

##
## PART II: ESTIMATION
##

if(PACK=="INLA") {
    if(exists("TPinla")) rm(TPinla)
                                        # create dataset with positive values only for the continuous part
    longDatlog <- longDat[longDat$Y>0,]

    nB <- length(longDat$Y) # length of binary part
    nC <- length(longDatlog$Y) # length of continuous part
    ns=dim(survDat)[1] # number of individuals

    longDat$B <- ifelse(longDat$Y==0,0,1) # zero value indicator (binary part outcome)
    longDatlog$sld <- longDatlog$Y # positive values only (continuous part outcome)

    ## response has two columns (binary and continuous part of the two-part model)
    ## and the number of lines corresponds to the sizes 
    ## of the binary, continuous and survival parts.
    yy <- matrix(NA, ncol = 2, nrow = nB+nC+ns)
    yy[1:nB,1] <- longDat$B # binary outcome
    yy[nB+(1:nC),2] <- longDatlog$Y # continuous outcome

                                        # fixed effects
    linear.covariate <- data.frame(
        Inte = as.factor(c(rep(1, nB), rep(2,nC), rep(0, ns))), # intercept (common)
        TIME = c(rep(0,nB),longDatlog$timej,rep(0,ns)), # time (continuous part)
        TIMEb = c(longDat$timej,rep(0,nC),rep(0,ns)), # time (binary part)
        TRTc = c(rep(0,nB),as.numeric(longDatlog$trtY)-1,rep(0,ns)), # treatment (continuous)
        TRTb = c(as.numeric(longDat$trtY)-1,rep(0,nC),rep(0,ns)), # treatment (binary)
        TRTs = c(rep(0,nC+nB),as.numeric(survDat$trt)-1)) # treatment (survival)

    ## set up unique identifiers for the random-effects
    longDatlog$idl <- ns+as.integer(longDatlog$id)
    longDatlog$idl2 <- ns+ns+as.integer(longDatlog$id)
    survDat$idl <- ns+as.integer(survDat$id)
    survDat$idl2 <- ns+ns+as.integer(survDat$id)

    ## random-effects
    random.covariate<-list(IDl=c(rep(NA,nB),longDatlog$idl,rep(NA,ns)), # random intercept (continuous)
                           IDs=c(rep(NA,nC+nB),survDat$idl), # association random intercept (continuous) with survival
                           IDb=c(as.integer(longDat$id),rep(NA,nC),rep(NA,ns)), # random intercept (binary)
                           IDsb=c(rep(NA,nC+nB),as.integer(survDat$id)), # association random intercept (binary)
                           IDl2=c(rep(NA,nB),as.integer(longDatlog$idl2),rep(NA,ns)), # random slope (continuous)
                           IDs2=c(rep(NA,nC+nB),as.integer(survDat$idl2)), # association random slope (continuous)
                           slopeCont=c(rep(NA,nB),longDatlog$timej,rep(NA,ns))) # weight for random slope (continuous)

    ## weight slope with deathtimes in survival model ? (not used)
    survD <- c(rep(NA,nC+nB),survDat$deathTimes) 

    joint.data <- c(linear.covariate,random.covariate)
    Yjoint = list(yy[,1],yy[,2], inla.surv(time=c(rep(NA,nC+nB),survDat$deathTimes),
                                           event=c(rep(NA,nC+nB),survDat$d))) # survival outcome
    joint.data$Y <- Yjoint

    ## conditional two-part joint model formula
    formulaJ= Yjoint ~ -1+Inte+TIME*TRTc+TIMEb*TRTb+TRTs+
        f(IDb, model="iid3d", n=3*(ns), constr=T)+
        f(IDl,copy="IDb")+
        f(IDl2, slopeCont,copy="IDb")+
        f(IDsb,copy="IDb",fixed=F)+
        f(IDs,copy="IDl",fixed=F)+
        f(IDs2,copy="IDl2",fixed=F)

    inla.setOption(num.threads = 6)
    inla.setOption(inla.call = "inla.mkl.work")
    h <- 0.005
    Sys.unsetenv("INLA_OPT_DIR")
    r <- inla(formulaJ,family = c("binomial", "gamma", "weibullsurv"), 
              data=joint.data,
              Ntrials=c(rep(1,length(longDat$Y)),rep(NA,nC),rep(NA,ns)),
              control.predictor=list(compute=TRUE,link=1),#error gaussian
              control.family=list(list(control.link = list(model = "logit")),
                                  list(link='log',hyper = list(prec = list(initial = 2, fixed=FALSE))),
                                  list(variant = 1)),
              control.inla = list(strategy="adaptive", h = h, int.strategy="eb"), 
              control.compute=list(openmp.strategy="pardiso.serial"),
              verbose=T)
    Sys.unsetenv("INLA_OPT_DIR")
    rr <- inla(formulaJ,family = c("binomial", "gamma", "weibullsurv"), 
               data=joint.data,
               Ntrials=c(rep(1,length(longDat$Y)),rep(NA,nC),rep(NA,ns)),
               control.predictor=list(compute=TRUE,link=1),#error gaussian
               control.family=list(list(control.link = list(model = "logit")),
                                   list(link='log',hyper = list(prec = list(initial = 2, fixed=FALSE))),
                                   list(variant = 1)),
               control.inla = list(strategy="adaptive", h = h, int.strategy="eb", optimise.strategy = "smart"), 
               control.compute=list(openmp.strategy="pardiso.serial"),
               verbose=T)
    Sys.setenv("INLA_OPT_DIR" = 1)
    rrr <- inla(formulaJ,family = c("binomial", "gamma", "weibullsurv"), 
                data=joint.data,
                Ntrials=c(rep(1,length(longDat$Y)),rep(NA,nC),rep(NA,ns)),
                control.predictor=list(compute=TRUE,link=1),#error gaussian
                control.family=list(list(control.link = list(model = "logit")),
                                    list(link='log',hyper = list(prec = list(initial = 2, fixed=FALSE))),
                                    list(variant = 1)),
                control.inla = list(strategy="adaptive", h = h, int.strategy="eb", optimise.strategy = "smart"), 
                control.compute=list(openmp.strategy="pardiso.serial"),
                verbose=T)



    print(round(dig = 4, r$misc$cor.intern))
    print(round(dig = 4, rr$misc$cor.intern))
    print(round(dig = 4, rrr$misc$cor.intern))

    print(c(r$cpu[2], rr$cpu[2], rrr$cpu[2]))
    print(c(r$misc$nfunc, rr$misc$nfunc,  rrr$misc$nfunc))
    print(c(r$mlik[1], rr$mlik[1], rrr$mlik[1]))
} 
