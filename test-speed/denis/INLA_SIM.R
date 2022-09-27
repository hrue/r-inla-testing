SAVE_res=F # save results (creates .RDATA file)

# Simulations for a joint model with 3 longitudinal markers and a terminal event 
# (int = Random intercept, slo = Random slope, binX = binary covariate, ctsX = continuous covariate)
# Gaussian(Y1(t)) = int1 + slo1 * t + Y1_intercept + Y1_slope * t + Y1_binX * binX + Y1_ctsX * ctsX
# Poisson(Y2(t)) = int2 + slo2 * t + Y2_intercept + Y2_slope * t + Y2_binX * binX + Y2_ctsX * ctsX
# Logit(Y3(t)) = int3 + slo3 * t + Y3_intercept + Y3_slope * t + Y3_binX * binX + Y3_ctsX * ctsX
# risk(t) = risk0(t) * exp(Surv_binX * binX + Surv_ctsX * ctsX + Asso_Y1 * E[Y1(t)] + Asso_Y2 * E[Y2(t)] + Asso_Y3 * E[Y3(t)])

library(INLA)
library(PermAlgo)
library(mvtnorm)
inla.setOption(inla.mode="experimental")
##inla.setOption(inla.call="inla.mkl.work")
inla.setOption(inla.call="remote")
inla.setOption(num.threads = "15:1")
set.seed(1) # seed for data generation
nmod <- 300 # number of models simulated
Startmod <- 0 # to start at a specific model
####
# 1 # data simulation
#####
# Need to sync random number generator because of some changes in R 3.6
nsujet=500 # number of indivduals

# Biomarker 1
b1_0=0.2 # intercept
b1_1=-0.1 # slope
b1_2=0.1 # continuous covariate
b1_3=-0.2 # binary covariate
b1_e=0.4 # residual error

# Biomarker 2
b2_0=0.2 # intercept
b2_1=-0.1 # slope
b2_2=0.1 # continuous covariate
b2_3=-0.2 # binary covariate
b2_e=0.4 # residual error

# Biomarker 3
b3_0=0.2 # intercept
b3_1=-0.1 # slope
b3_2=0.1 # continuous covariate
b3_3=-0.2 # binary covariate
b3_e=0.4 # residual error



# survival part
b1_s=0.5 # effect of biomarker 1 on the risk of event
b2_s=-0.5 # effect of biomarker 2 on the risk of event
b3_s=0.5 # effect of biomarker 3 on the risk of event

gapLongi=0.3 # gap between longi measurements
gap=0.01 # used to generate a lot of time points because the permutation
# algorithm choses among those time points to define survival times
followup=5 # follow-up time

# random effects variance and covariance
b1_int=0.4 # random intercept biomarker 1
b1_slo=0.4 # random slope biomarker 1
b2_int=0.5 # random intercept biomarker 2
b2_slo=0.5 # random slope biomarker 2
b3_int=0.5 # random intercept biomarker 3
b3_slo=0.4 # random slope biomarker 3
cor_b1intslo=0.5
cor_b1intb2int=0.1
cor_b1intb2slo=0.2
cor_b1intb3int=0
cor_b1intb3slo=-0.25
cor_b1slob2int=0.2
cor_b1slob2slo=0
cor_b1slob3int=-0.4
cor_b1slob3slo=-0.5
cor_b2intslo=0.4
cor_b2intb3int=0.2
cor_b2intb3slo=0.1
cor_b2slob3int=0.2
cor_b2slob3slo=-0.2
cor_b3intslo=0.5
cov_b1intslo <- b1_int*b1_slo*cor_b1intslo
cov_b1intb2int <- b1_int*b2_int*cor_b1intb2int
cov_b1intb2slo <- b1_int*b2_slo*cor_b1intb2slo
cov_b1intb3int <- b1_int*b3_int*cor_b1intb3int
cov_b1intb3slo <- b1_int*b3_slo*cor_b1intb3slo
cov_b1slob2int <- b1_slo*b2_int*cor_b1slob2int
cov_b1slob2slo <- b1_slo*b2_slo*cor_b1slob2slo
cov_b1slob3int <- b1_slo*b3_int*cor_b1slob3int
cov_b1slob3slo <- b1_slo*b3_slo*cor_b1slob3slo
cov_b2intslo <- b2_int*b2_slo*cor_b2intslo
cov_b2intb3int <- b2_int*b3_int*cor_b2intb3int
cov_b2slob3int <- b2_slo*b3_int*cor_b2slob3int
cov_b2intb3slo <- b2_int*b3_slo*cor_b2intb3slo
cov_b2slob3slo <- b2_slo*b3_slo*cor_b2slob3slo
cov_b3intslo <- b3_int*b3_slo*cor_b3intslo

Sigma=matrix(c(b1_int^2,cov_b1intslo,cov_b1intb2int,cov_b1intb2slo,cov_b1intb3int,cov_b1intb3slo,
               cov_b1intslo,b1_slo^2,cov_b1slob2int,cov_b1slob2slo,cov_b1slob3int,cov_b1slob3slo,
               cov_b1intb2int,cov_b1slob2int,b2_int^2,cov_b2intslo,cov_b2intb3int,cov_b2intb3slo,
               cov_b1intb2slo,cov_b1slob2slo,cov_b2intslo,b2_slo^2,cov_b2slob3int,cov_b2slob3slo,
               cov_b1intb3int,cov_b1slob3int,cov_b2intb3int,cov_b2slob3int,b3_int^2,cov_b3intslo,
               cov_b1intb3slo,cov_b1slob3slo,cov_b2intb3slo,cov_b2slob3slo,cov_b3intslo,b3_slo^2),ncol=6,nrow=6)

mestime=seq(0,followup,gap) # measurement times
timej=rep(mestime, nsujet) # time column 
nmesindiv=followup/gap+1 # number of individual measurements

nmesy= nmesindiv*nsujet# number of longi measurements
idY<-as.factor(rep(1:nsujet, each=nmesindiv)) # id

# store results in object "res"
cols <- c("True Val.","INLA", "INLA_sd","INLA_025","INLA_975")
rows <- c("Y1_intercept", "Y1_slope", "Y1_ctsX", "Y1_binX","Y1_err",
          "Y2_intercept", "Y2_slope", "Y2_ctsX", "Y2_binX","Y2_err",
          "Y3_intercept", "Y3_slope", "Y3_ctsX", "Y3_binX","Y3_err",
          "var_int1", "var_slo1", "var_int2", "var_slo2",  "var_int3", "var_slo3", 
          "cov_i1s1", "cov_i1i2", "cov_i1s2", "cov_i1i3", "cov_i1s3", 
          "cov_s1i2","cov_s1s2","cov_s1i3","cov_s1s3",
          "cov_i2s2","cov_i2i3","cov_i2s3",
          "cov_s2i3","cov_s2s3",
          "cov_i3s3",
          "Asso_Y1", "Asso_Y2", "Asso_Y3",
          "Comp. time (sec.)"
)
res=matrix(data = NA, ncol=length(cols), nrow=length(rows))
colnames(res) <- cols
rownames(res) <- rows
res[,"True Val."] <- c(b1_0, b1_1, b1_2, b1_3,b1_e,
                       b2_0, b2_1, b2_2, b2_3,b2_e,
                       b3_0, b3_1, b3_2, b3_3,b3_e,
                       diag(Sigma), Sigma[lower.tri(Sigma)], b1_s, b2_s, b3_s, 0)
res_list_INLA <- replicate(nmod, res, simplify = FALSE) # empty list of results for each model
JMinla <- NULL



for(i in 1:nmod){
  skip_to_next <- FALSE
  print(paste("########################### Model: ", i, "/", nmod))
  set.seed(i) # seed for data generation
  JMinla <- NULL

  ### begin data generation
  # random effects generation
  MVnorm <- rmvnorm(nsujet, rep(0, 6), Sigma)
  
  b1_int = MVnorm[,1]
  b1_intY <- rep(b1_int, each=nmesindiv)
  b1_slo = MVnorm[,2]
  b1_sloY <- rep(b1_slo, each=nmesindiv)
  b2_int = MVnorm[,3]
  b2_intY <- rep(b2_int, each=nmesindiv)
  b2_slo = MVnorm[,4]
  b2_sloY <- rep(b2_slo, each=nmesindiv)
  b3_int = MVnorm[,5]
  b3_intY <- rep(b3_int, each=nmesindiv)
  b3_slo = MVnorm[,6]
  b3_sloY <- rep(b3_slo, each=nmesindiv)
  
  binX=rbinom(nsujet,1, 0.5) # binary covariate
  binXY=rep(binX, each=nmesindiv)
  
  ctsX=rnorm(nsujet,1, 0.5) # continuous covariate
  ctsXY=rep(ctsX, each=nmesindiv)
  
  
  # linear predictors
  linPred1 <- b1_0+b1_intY+(b1_1+b1_sloY)*timej+b1_2*ctsXY+b1_3*binXY
  linPred2 <- b2_0+b2_intY+(b2_1+b2_sloY)*timej+b2_2*ctsXY+b2_3*binXY
  linPred3 <- b3_0+b3_intY+(b3_1+b3_sloY)*timej+b3_2*ctsXY+b3_3*binXY
  
  # continuous outcome (biomarker 1)
  biomarker1 <- rnorm(nmesy, linPred1, b1_e) 

  # continuous outcome (biomarker 2)
  biomarker2 <- rnorm(nmesy, linPred2, b2_e) 
  
  # continuous outcome (biomarker 3)
  biomarker3 <- rnorm(nmesy, linPred3, b3_e) 
  
  
  #longitudinal dataset
  id <- as.integer(idY)
  longDat <- data.frame(id, timej, linPred1, linPred2, linPred3)
  
  Uid <- paste(idY,timej) # unique identifier for each row
  Correspid <- cbind(Uid, ctsXY, binXY, biomarker1, biomarker2, biomarker3) # id associated to outcomes and covariates
  CorrespidSurv <- cbind("expand..coxph"=1:nsujet, ctsX, binX) # id associated to outcomes and covariates
  
  # longi measurements to generate survival times with permutation algorithm
  matX=matrix(ncol=3, nrow=nsujet*nmesindiv)  
  matX[,1] <- longDat[,"linPred1"] 
  matX[,2] <- longDat[,"linPred2"] 
  matX[,3] <- longDat[,"linPred3"] 
  eventRandom <- round(rexp(nsujet, 0.0025)+1,0) # ~40% death
  censorRandom=runif(nsujet,1,nmesindiv) # uniform random censoring
  Ttemp <- permalgorithm(nsujet,nmesindiv,Xmat=matX,eventRandom = eventRandom,
                         censorRandom=censorRandom,XmatNames=c("linPred1", "linPred2", "linPred3"), 
                         betas=c(b1_s,b2_s,b3_s))
  
  # extract last line of each individual (= death/censoring time)
  ligne <- c(which(diff(Ttemp[,"Id"])==1), dim(Ttemp)[1])
  
  Ttemp2=Ttemp[ligne, c("Id","Event","Stop")] # one line per individual
  Ttemp2$deathTimes <- mestime[Ttemp2$Stop+1] # deathtimes
  survDat <- Ttemp2[, c("Id", "deathTimes", "Event")] # survival dataset
  names(survDat) <- c("id", "deathTimes", "d")
  
  longDat2 <- Ttemp[,c("Id", "Start")]
  longDat2$timej <- mestime[longDat2$Start+1] # measurements times of the biomarker
  names(longDat2) <- c("id", "Start", "timej")
  longDat2$Uid <- paste(longDat2$id, longDat2$timej) # unique identifier to add covariates and observed biomarker values
  longDat3 <- merge(longDat2, Correspid, by=c("Uid"))
  timesLongi=mestime[which(round(mestime-round(mestime/gapLongi,0)*gapLongi,6)==0)] # visit times
  longDat <- longDat3[longDat3$timej%in%timesLongi,-c(1,3)]
  
  survDat$id <- as.integer(survDat$id)
  longDat$ctsXY <- as.numeric(longDat$ctsXY)
  longDat[, c("binXY", "biomarker1", "biomarker2", "biomarker3")] <- apply(longDat[, c("binXY", "biomarker1", "biomarker2", "biomarker3")], 2, as.numeric)
  
  colnames(longDat) <- c("id", "timej","ctsX", "binX", "Y1", "Y2", "Y3")
  colnames(survDat) <- c("id", "deathTimes", "d")
  longDat <- longDat[order(longDat$id, longDat$timej),]
  survDat <- survDat[order(survDat$id),]
  
  ### datasets generated
  
  print(head(longDat, 20))
  print(head(survDat, 20))
  print(str(survDat))
  print(str(longDat))
  print(summary(survDat))
  print(summary(longDat))
  
  
  
  # INLA
  if(Startmod!=0){ # Start from a specific model
    if(i==Startmod-1){
        load("./res_list_INLA.RData") # load previous results
      Startmod <- 0
    }
  }else{
    NL <- length(longDat$Y1)
    NS <- length(survDat$id)
  Y1 <- c(longDat$Y1, rep(NA, NL*2+NS*3))
  Y.eta1 <- c(rep(NA, NL), rep(0, NS),rep(NA, NL*2+NS*2))
  
  Y2 <- c(rep(NA, NL+NS), longDat$Y2, rep(NA, NL+NS*2))
  Y.eta2 <- c(rep(NA, NL*2+NS), rep(0, NS), rep(NA, NL+NS))
  
  Y3 <- c(rep(NA, NL*2+NS*2), longDat$Y3, rep(NA, NS))
  Y.eta3 <- c(rep(NA, NL*3+NS*2), rep(0, NS))
  
  Y.joint <- list(Y1,Y.eta1,Y2,Y.eta2,Y3,Y.eta3)
    
    
    YS <- inla.surv(time = c(survDat$deathTimes),
                    event = c(survDat$d))
    NintBH=15
    cox_ext = inla.coxph(YS ~  -1+Intercept,
                         control.hazard=list(model="rw2", 
                                             scale.model=TRUE,
                                             diagonal=1e-2,
                                             constr=TRUE,
                                             n.intervals=NintBH,
                                             hyper=list(prec=list(prior="pc.prec",
                                                                  param=c(0.5,0.01)))),
                         data = c(list(YS = YS,
                                     Intercept = rep(1, NS), 
                                     eta1 = NS*7+survDat$id,
                                     eta2 = NS*8+survDat$id,
                                     eta3 = NS*9+survDat$id)))
    
    # time weight for time-dependent covariates
    re.weight <- cox_ext$data$baseline.hazard.time +  0.5 * cox_ext$data$baseline.hazard.length
    ns_cox = dim(cox_ext$data)[1]
    IDcox1 <- cox_ext$data$eta1
    IDcox2 <- cox_ext$data$eta2
    IDcox3 <- cox_ext$data$eta3
    cox_ext$data$eta1 <- NS*6+1:ns_cox
    cox_ext$data$eta2 <- NS*6+ns_cox+1:ns_cox
    cox_ext$data$eta3 <- NS*6+ns_cox*2+1:ns_cox
    
    # match binary and continuous covariates with survival time intervals
    IDBC <- merge(cox_ext$data, CorrespidSurv, by="expand..coxph")
    
    linear.covariate <- data.frame(
    InteY1 = c(rep(1,NL+ns_cox), rep(NA, ns_cox*2+NL*2)), # intercept Y1
    TIMEY1 = c(longDat$timej,rep(NA,ns_cox), rep(NA, ns_cox*2+NL*2)), # time Y1
    binXY1 = c(longDat$binX, IDBC$binX, rep(NA, ns_cox*2+NL*2)), # binX Y1
    ctsXY1 = c(longDat$ctsX, IDBC$ctsX, rep(NA, ns_cox*2+NL*2)), # ctsX Y1
    InteY2 = c(rep(NA,NL+ns_cox), rep(1, NL+ns_cox), rep(NA, ns_cox+NL)), # intercept Y2
    TIMEY2 = c(rep(NA,NL+ns_cox), longDat$timej, rep(NA,ns_cox), rep(NA, ns_cox+NL)), # time Y2
    binXY2 = c(rep(NA,NL+ns_cox), longDat$binX, IDBC$binX, rep(NA, ns_cox+NL)), # binX Y2
    ctsXY2 = c(rep(NA,NL+ns_cox), longDat$ctsX, IDBC$ctsX, rep(NA, ns_cox+NL)), # ctsX Y2
    InteY3 = c(rep(NA,NL*2+ns_cox*2), rep(1, NL+ns_cox)), # intercept Y3
    TIMEY3 = c(rep(NA,NL*2+ns_cox*2), longDat$timej, rep(NA,ns_cox)), # time Y3
    binXY3 = c(rep(NA,NL*2+ns_cox*2), longDat$binX, IDBC$binX), # binX Y3
    ctsXY3 = c(rep(NA,NL*2+ns_cox*2), longDat$ctsX, IDBC$ctsX)) # ctsX Y3
  random.covariate<-list(IDY1=c(longDat$id, IDcox1-NS*7, rep(NA, NL*2+ns_cox*2)), # random intercept Y1
                         IDY1_s=c(NS+longDat$id, IDcox1-NS*6, rep(NA, NL*2+ns_cox*2)), # random slope Y1
                         WY1_s=c(longDat$timej, rep(NA,ns_cox), rep(NA, NL*2+ns_cox*2)), # weight random slope Y1
                         IDY2=c(rep(NA, NL+ns_cox), NS*2+longDat$id, IDcox1-NS*5, rep(NA, NL+ns_cox)), # random intercept Y2
                         IDY2_s=c(rep(NA, NL+ns_cox), NS*3+longDat$id, IDcox1-NS*4, rep(NA, NL+ns_cox)), # random slope Y2
                         WY2_s=c(rep(NA, NL+ns_cox), longDat$timej, rep(NA,ns_cox), rep(NA, NL+ns_cox)), # weight random slope Y2
                         IDY3=c(rep(NA, NL*2+ns_cox*2), NS*4+longDat$id,  IDcox1-NS*3), # random intercept Y3
                         IDY3_s=c(rep(NA, NL*2+ns_cox*2), NS*5+longDat$id,IDcox1-NS*2), # random slope Y3
                         WY3_s=c(rep(NA, NL*2+ns_cox*2), longDat$timej, rep(NA,ns_cox)), # weight random slope Y3
                         u1 = c(rep(NA, NL), cox_ext$data$eta1, rep(NA, NL*2+ns_cox*2)), # Y1 association survival
                         w1 = c(rep(NA, NL), rep(-1, ns_cox), rep(NA, NL*2+ns_cox*2)), # Y1 weight association survival
                         u2 = c(rep(NA, NL*2+ns_cox), cox_ext$data$eta2, rep(NA, NL+ns_cox)), # Y2 association survival
                         w2 = c(rep(NA, NL*2+ns_cox), rep(-1, ns_cox), rep(NA, NL+ns_cox)), # Y2 weight association survival
                         u3 = c(rep(NA, NL*3+ns_cox*2), cox_ext$data$eta3), # Y3 association survival
                         w3 = c(rep(NA, NL*3+ns_cox*2), rep(-1, ns_cox))) # Y3 weight association survival
  
  Y1 <- c(longDat$Y1, rep(NA, NL*2+ns_cox*3))
  Y.eta1 <- c(rep(NA, NL), rep(0, ns_cox),rep(NA, NL*2+ns_cox*2))
  
  Y2 <- c(rep(NA, NL+ns_cox), longDat$Y2,rep(NA, NL+ns_cox*2))
  Y.eta2 <- c(rep(NA, NL*2+ns_cox), rep(0, ns_cox),rep(NA, NL+ns_cox))
  
  Y3 <- c(rep(NA, NL*2+ns_cox*2), longDat$Y3,rep(NA, ns_cox))
  Y.eta3 <- c(rep(NA, NL*3+ns_cox*2), rep(0, ns_cox))
  
  Y.joint <- list(Y1,Y.eta1,Y2,Y.eta2,Y3,Y.eta3)
  
  
  jointdf = data.frame(linear.covariate, random.covariate, Y1,Y.eta1, Y2, Y.eta2,Y3,Y.eta3)
  joint.data_cox <- c(as.list(inla.rbind.data.frames(jointdf, cox_ext$data)), 
                      cox_ext$data.list)
  Yjoint = cbind(joint.data_cox$Y1, joint.data_cox$Y.eta1, joint.data_cox$Y2, joint.data_cox$Y.eta2, joint.data_cox$Y3, joint.data_cox$Y.eta3, joint.data_cox$y..coxph) # outcomes (longi and survival)
  joint.data_cox$Y <- Yjoint






  # update from the cox expansion
  formulaJ= update(cox_ext$formula, Yjoint ~ . -1 + InteY1 + TIMEY1+ binXY1 + ctsXY1  +
                     InteY2 + TIMEY2 + binXY2 + ctsXY2 + 
                     InteY3 + TIMEY3 + binXY3 + ctsXY3 +
                     f(IDY1, model="iidkd", order=6, n=NS*6,constr=FALSE, 
                       hyper = list(theta1 = list(param = c(30,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))))+
                     f(IDY1_s, WY1_s, copy="IDY1")+
                     f(IDY2, copy="IDY1")+
                     f(IDY2_s, WY2_s, copy="IDY1")+
                     f(IDY3, copy="IDY1")+
                     f(IDY3_s, WY3_s, copy="IDY1")+
                     f(u1, w1, model="iid", hyper = list(prec = list(initial = -6, fixed=TRUE)), constr=F)+
                     f(eta1, copy="u1", hyper = list(beta = list(fixed = FALSE,
                                                                 param = c(0, 0.16), 
                                                                 initial = 1)))+
                     f(u2, w2, model="iid", hyper = list(prec = list(initial = -6, fixed=TRUE)), constr=F)+
                     f(eta2, copy="u2", hyper = list(beta = list(fixed = FALSE,
                                                                 param = c(0, 0.16), 
                                                                 initial = 1)))+
                     f(u3, w3, model="iid", hyper = list(prec = list(initial = -6, fixed=TRUE)), constr=F)+
                     f(eta3, copy="u3", hyper = list(beta = list(fixed = FALSE,
                                                                 param = c(0, 0.16), 
                                                                 initial = 1))))
  
  
  
    NT <- length(joint.data_cox$WY1_s)
    # account for random slope of time in survival
    joint.data_cox$WY1_s[(NL+1):(NL+ns_cox)] <- re.weight 
    joint.data_cox$WY2_s[(NL*2+ns_cox+1):(NL*2+ns_cox*2)] <- re.weight
    joint.data_cox$WY3_s[(NL*3+ns_cox*2+1):(NL*3+ns_cox*3)] <- re.weight
    joint.data_cox$TIMEY1[(NL+1):(NL+ns_cox)] <- re.weight
    joint.data_cox$TIMEY2[(NL*2+ns_cox+1):(NL*2+ns_cox*2)] <- re.weight
    joint.data_cox$TIMEY3[(NL*3+ns_cox*2+1):(NL*3+ns_cox*3)] <- re.weight
    #joint.data_cox$E..coxph[(NL+ns_cox+1):(NL*2+ns_cox)] <- 1
  
    rer=0
    while(class(JMinla)!="inla" & rer<=20){
      if(rer!=0) print(paste("ERROR, try ", rer))
      rer=rer+1
  a_INLA <- Sys.time()
  JMinla <- try(inla(formulaJ,family = c("gaussian", "gaussian", "gaussian", "gaussian", "gaussian", "gaussian", cox_ext$family), 
                 data=joint.data_cox,
                 control.fixed = list(mean=0, prec=0.16, mean.intercept=0, prec.intercept=0.16), 
                 control.family = list(
                   list(),
                   list(hyper = list(prec = list(initial = 12, fixed=TRUE))),
                   list(),
                   list(hyper = list(prec = list(initial = 12, fixed=TRUE))),
                   list(),
                   list(hyper = list(prec = list(initial = 12, fixed=TRUE))),
                   list()
                 ),
                 E = joint.data_cox$E..coxph,
                 control.inla = list(parallel.linesearch=F, control.vb = list(f.enable.limit = 50), cmin = 0, verbose = F,int.strategy="eb"), 
                 verbose = T, safe=T))
      stop("XXXXXXXXXXXXXXXXX")
}
        
        b_INLA <- Sys.time()
    if(class(JMinla)=="inla") {
      
    if(!("Inf" %in% JMinla$summary.hyperpar[-which(rownames(JMinla$summary.hyperpar)=="Precision for baseline.hazard"), "mean"] | "NaN" %in% JMinla$summary.hyperpar[, "mean"])){
      
      
      m.lstat.1 <- function(m) { #SD
        moments <- inla.emarginal(function(lx) c(exp(-lx/2), exp(-lx)), m)
        #q = inla.qmarginal(c(0.025,0.975), exp(-m/2))
        q = exp(-inla.qmarginal(c(0.025,0.975), m)/2)
        return(list(mean = moments[1], sd = sqrt(max(0, moments[2]-moments[1]^2)), "0.025quant"=q[2], "0.975quant"=q[1]))
      }
      m.lstat.2 <- function(m) { #Variance
        moments <- inla.emarginal(function(lx) c(exp(-lx), exp(-2*lx)), m)
        q = exp(-inla.qmarginal(c(0.025,0.975), m))
        return(list(mean = moments[1], sd = sqrt(max(0, moments[2]-moments[1]^2)), "0.025quant"=q[2], "0.975quant"=q[1]))
      }
    
      #MC_samples <- inla.hyperpar.sample(10000, JMinla) # for random-effects covariance terms
      MC_samples <- inla.iidkd.sample(10^4, JMinla, "IDY1", return.cov=TRUE) # for random-effects covariance terms
      
      # residual error biomarker 1
      resErr1_stat <- m.lstat.1(JMinla$internal.marginals.hyperpar$`Log precision for the Gaussian observations`)
      resErr2_stat <- m.lstat.1(JMinla$internal.marginals.hyperpar$`Log precision for the Gaussian observations[3]`)
      resErr3_stat <- m.lstat.1(JMinla$internal.marginals.hyperpar$`Log precision for the Gaussian observations[5]`)
      
      # random-effects variance-covariance matrix (6x6)
      VarCov <- matrix(unlist(MC_samples), nrow = 6^2)
      VarCovMeans <- matrix(rowMeans(VarCov),6,6)
      VarCovSD <- matrix(apply(VarCov,1,sd),6,6)
      VarCov025 <- matrix(apply(VarCov,1,function(x) quantile(x, 0.025)),6,6)
      VarCov975 <- matrix(apply(VarCov,1,function(x) quantile(x, 0.975)),6,6)
      

      
      # fill the results
      res["Y1_intercept", "INLA"] <- JMinla$summary.fixed["InteY1", "mean"]
      res["Y1_intercept", "INLA_sd"] <- JMinla$summary.fixed["InteY1", "sd"]
      res["Y1_intercept", "INLA_025"] <- JMinla$summary.fixed["InteY1", "0.025quant"]
      res["Y1_intercept", "INLA_975"] <- JMinla$summary.fixed["InteY1", "0.975quant"]
      res["Y1_slope", "INLA"] <- JMinla$summary.fixed["TIMEY1", "mean"]
      res["Y1_slope", "INLA_sd"] <- JMinla$summary.fixed["TIMEY1", "sd"]
      res["Y1_slope", "INLA_025"] <- JMinla$summary.fixed["TIMEY1", "0.025quant"]
      res["Y1_slope", "INLA_975"] <- JMinla$summary.fixed["TIMEY1", "0.975quant"]
      res["Y1_binX", "INLA"] <- JMinla$summary.fixed["binXY1", "mean"]
      res["Y1_binX", "INLA_sd"] <- JMinla$summary.fixed["binXY1", "sd"]
      res["Y1_binX", "INLA_025"] <- JMinla$summary.fixed["binXY1", "0.025quant"]
      res["Y1_binX", "INLA_975"] <- JMinla$summary.fixed["binXY1", "0.975quant"]
      res["Y1_ctsX", "INLA"] <- JMinla$summary.fixed["ctsXY1", "mean"]
      res["Y1_ctsX", "INLA_sd"] <- JMinla$summary.fixed["ctsXY1", "sd"]
      res["Y1_ctsX", "INLA_025"] <- JMinla$summary.fixed["ctsXY1", "0.025quant"]
      res["Y1_ctsX", "INLA_975"] <- JMinla$summary.fixed["ctsXY1", "0.975quant"]
      res["Y1_err", "INLA"] <- resErr1_stat$mean
      res["Y1_err", "INLA_sd"] <- resErr1_stat$sd
      res["Y1_err", "INLA_025"] <- resErr1_stat$`0.025quant`
      res["Y1_err", "INLA_975"] <- resErr1_stat$`0.975quant`
      res["Y2_intercept", "INLA"] <- JMinla$summary.fixed["InteY2", "mean"]
      res["Y2_intercept", "INLA_sd"] <- JMinla$summary.fixed["InteY2", "sd"]
      res["Y2_intercept", "INLA_025"] <- JMinla$summary.fixed["InteY2", "0.025quant"]
      res["Y2_intercept", "INLA_975"] <- JMinla$summary.fixed["InteY2", "0.975quant"]
      res["Y2_slope", "INLA"] <- JMinla$summary.fixed["TIMEY2", "mean"]
      res["Y2_slope", "INLA_sd"] <- JMinla$summary.fixed["TIMEY2", "sd"]
      res["Y2_slope", "INLA_025"] <- JMinla$summary.fixed["TIMEY2", "0.025quant"]
      res["Y2_slope", "INLA_975"] <- JMinla$summary.fixed["TIMEY2", "0.975quant"]
      res["Y2_binX", "INLA"] <- JMinla$summary.fixed["binXY2", "mean"]
      res["Y2_binX", "INLA_sd"] <- JMinla$summary.fixed["binXY2", "sd"]
      res["Y2_binX", "INLA_025"] <- JMinla$summary.fixed["binXY2", "0.025quant"]
      res["Y2_binX", "INLA_975"] <- JMinla$summary.fixed["binXY2", "0.975quant"]
      res["Y2_ctsX", "INLA"] <- JMinla$summary.fixed["ctsXY2", "mean"]
      res["Y2_ctsX", "INLA_sd"] <- JMinla$summary.fixed["ctsXY2", "sd"]
      res["Y2_ctsX", "INLA_025"] <- JMinla$summary.fixed["ctsXY2", "0.025quant"]
      res["Y2_ctsX", "INLA_975"] <- JMinla$summary.fixed["ctsXY2", "0.975quant"]
      res["Y2_err", "INLA"] <- resErr2_stat$mean
      res["Y2_err", "INLA_sd"] <- resErr2_stat$sd
      res["Y2_err", "INLA_025"] <- resErr2_stat$`0.025quant`
      res["Y2_err", "INLA_975"] <- resErr2_stat$`0.975quant`
      res["Y3_intercept", "INLA"] <- JMinla$summary.fixed["InteY3", "mean"]
      res["Y3_intercept", "INLA_sd"] <- JMinla$summary.fixed["InteY3", "sd"]
      res["Y3_intercept", "INLA_025"] <- JMinla$summary.fixed["InteY3", "0.025quant"]
      res["Y3_intercept", "INLA_975"] <- JMinla$summary.fixed["InteY3", "0.975quant"]
      res["Y3_slope", "INLA"] <- JMinla$summary.fixed["TIMEY3", "mean"]
      res["Y3_slope", "INLA_sd"] <- JMinla$summary.fixed["TIMEY3", "sd"]
      res["Y3_slope", "INLA_025"] <- JMinla$summary.fixed["TIMEY3", "0.025quant"]
      res["Y3_slope", "INLA_975"] <- JMinla$summary.fixed["TIMEY3", "0.975quant"]
      res["Y3_binX", "INLA"] <- JMinla$summary.fixed["binXY3", "mean"]
      res["Y3_binX", "INLA_sd"] <- JMinla$summary.fixed["binXY3", "sd"]
      res["Y3_binX", "INLA_025"] <- JMinla$summary.fixed["binXY3", "0.025quant"]
      res["Y3_binX", "INLA_975"] <- JMinla$summary.fixed["binXY3", "0.975quant"]
      res["Y3_ctsX", "INLA"] <- JMinla$summary.fixed["ctsXY3", "mean"]
      res["Y3_ctsX", "INLA_sd"] <- JMinla$summary.fixed["ctsXY3", "sd"]
      res["Y3_ctsX", "INLA_025"] <- JMinla$summary.fixed["ctsXY3", "0.025quant"]
      res["Y3_ctsX", "INLA_975"] <- JMinla$summary.fixed["ctsXY3", "0.975quant"]
      res["Y3_err", "INLA"] <- resErr3_stat$mean
      res["Y3_err", "INLA_sd"] <- resErr3_stat$sd
      res["Y3_err", "INLA_025"] <- resErr3_stat$`0.025quant`
      res["Y3_err", "INLA_975"] <- resErr3_stat$`0.975quant`
      res["var_int1", "INLA"] <- VarCovMeans[1,1]
      res["var_int1", "INLA_sd"] <- VarCovSD[1,1]
      res["var_int1", "INLA_025"] <- VarCov025[1,1]
      res["var_int1", "INLA_975"] <- VarCov975[1,1]
      res["var_slo1", "INLA"] <- VarCovMeans[2,2]
      res["var_slo1", "INLA_sd"] <- VarCovSD[2,2]
      res["var_slo1", "INLA_025"] <- VarCov025[2,2]
      res["var_slo1", "INLA_975"] <- VarCov975[2,2]
      res["var_int2", "INLA"] <- VarCovMeans[3,3]
      res["var_int2", "INLA_sd"] <- VarCovSD[3,3]
      res["var_int2", "INLA_025"] <- VarCov025[3,3]
      res["var_int2", "INLA_975"] <- VarCov975[3,3]
      res["var_slo2", "INLA"] <- VarCovMeans[4,4]
      res["var_slo2", "INLA_sd"] <- VarCovSD[4,4]
      res["var_slo2", "INLA_025"] <- VarCov025[4,4]
      res["var_slo2", "INLA_975"] <- VarCov975[4,4]
      res["var_int3", "INLA"] <- VarCovMeans[5,5]
      res["var_int3", "INLA_sd"] <- VarCovSD[5,5]
      res["var_int3", "INLA_025"] <- VarCov025[5,5]
      res["var_int3", "INLA_975"] <- VarCov975[5,5]
      res["var_slo3", "INLA"] <- VarCovMeans[6,6]
      res["var_slo3", "INLA_sd"] <- VarCovSD[6,6]
      res["var_slo3", "INLA_025"] <- VarCov025[6,6]
      res["var_slo3", "INLA_975"] <- VarCov975[6,6]
      res["cov_i1s1", "INLA"] <- VarCovMeans[1,2]
      res["cov_i1s1", "INLA_sd"] <- VarCovSD[1,2]
      res["cov_i1s1", "INLA_025"] <- VarCov025[1,2]
      res["cov_i1s1", "INLA_975"] <- VarCov975[1,2]
      res["cov_i1i2", "INLA"] <- VarCovMeans[1,3]
      res["cov_i1i2", "INLA_sd"] <- VarCovSD[1,3]
      res["cov_i1i2", "INLA_025"] <- VarCov025[1,3]
      res["cov_i1i2", "INLA_975"] <- VarCov975[1,3]
      res["cov_i1s2", "INLA"] <- VarCovMeans[1,4]
      res["cov_i1s2", "INLA_sd"] <- VarCovSD[1,4]
      res["cov_i1s2", "INLA_025"] <- VarCov025[1,4]
      res["cov_i1s2", "INLA_975"] <- VarCov975[1,4]
      res["cov_i1i3", "INLA"] <- VarCovMeans[1,5]
      res["cov_i1i3", "INLA_sd"] <- VarCovSD[1,5]
      res["cov_i1i3", "INLA_025"] <- VarCov025[1,5]
      res["cov_i1i3", "INLA_975"] <- VarCov975[1,5]
      res["cov_i1s3", "INLA"] <- VarCovMeans[1,6]
      res["cov_i1s3", "INLA_sd"] <- VarCovSD[1,6]
      res["cov_i1s3", "INLA_025"] <- VarCov025[1,6]
      res["cov_i1s3", "INLA_975"] <- VarCov975[1,6]
      res["cov_s1i2", "INLA"] <- VarCovMeans[2,3]
      res["cov_s1i2", "INLA_sd"] <- VarCovSD[2,3]
      res["cov_s1i2", "INLA_025"] <- VarCov025[2,3]
      res["cov_s1i2", "INLA_975"] <- VarCov975[2,3]
      res["cov_s1s2", "INLA"] <- VarCovMeans[2,4]
      res["cov_s1s2", "INLA_sd"] <- VarCovSD[2,4]
      res["cov_s1s2", "INLA_025"] <- VarCov025[2,4]
      res["cov_s1s2", "INLA_975"] <- VarCov975[2,4]
      res["cov_s1i3", "INLA"] <- VarCovMeans[2,5]
      res["cov_s1i3", "INLA_sd"] <- VarCovSD[2,5]
      res["cov_s1i3", "INLA_025"] <- VarCov025[2,5]
      res["cov_s1i3", "INLA_975"] <- VarCov975[2,5]
      res["cov_s1s3", "INLA"] <- VarCovMeans[2,6]
      res["cov_s1s3", "INLA_sd"] <- VarCovSD[2,6]
      res["cov_s1s3", "INLA_025"] <- VarCov025[2,6]
      res["cov_s1s3", "INLA_975"] <- VarCov975[2,6]
      res["cov_i2s2", "INLA"] <- VarCovMeans[3,4]
      res["cov_i2s2", "INLA_sd"] <- VarCovSD[3,4]
      res["cov_i2s2", "INLA_025"] <- VarCov025[3,4]
      res["cov_i2s2", "INLA_975"] <- VarCov975[3,4]
      res["cov_i2i3", "INLA"] <- VarCovMeans[3,5]
      res["cov_i2i3", "INLA_sd"] <- VarCovSD[3,5]
      res["cov_i2i3", "INLA_025"] <- VarCov025[3,5]
      res["cov_i2i3", "INLA_975"] <- VarCov975[3,5]
      res["cov_i2s3", "INLA"] <- VarCovMeans[3,6]
      res["cov_i2s3", "INLA_sd"] <- VarCovSD[3,6]
      res["cov_i2s3", "INLA_025"] <- VarCov025[3,6]
      res["cov_i2s3", "INLA_975"] <- VarCov975[3,6]
      res["cov_s2i3", "INLA"] <- VarCovMeans[4,5]
      res["cov_s2i3", "INLA_sd"] <- VarCovSD[4,5]
      res["cov_s2i3", "INLA_025"] <- VarCov025[4,5]
      res["cov_s2i3", "INLA_975"] <- VarCov975[4,5]
      res["cov_s2s3", "INLA"] <- VarCovMeans[4,6]
      res["cov_s2s3", "INLA_sd"] <- VarCovSD[4,6]
      res["cov_s2s3", "INLA_025"] <- VarCov025[4,6]
      res["cov_s2s3", "INLA_975"] <- VarCov975[4,6]
      res["cov_i3s3", "INLA"] <- VarCovMeans[5,6]
      res["cov_i3s3", "INLA_sd"] <- VarCovSD[5,6]
      res["cov_i3s3", "INLA_025"] <- VarCov025[5,6]
      res["cov_i3s3", "INLA_975"] <- VarCov975[5,6]
      res["Asso_Y1", "INLA"] <- JMinla$summary.hyperpar["Beta for eta1", "mean"]
      res["Asso_Y1", "INLA_sd"] <- JMinla$summary.hyperpar["Beta for eta1", "sd"]
      res["Asso_Y1", "INLA_025"] <- JMinla$summary.hyperpar["Beta for eta1", "0.025quant"]
      res["Asso_Y1", "INLA_975"] <- JMinla$summary.hyperpar["Beta for eta1", "0.975quant"]
      res["Asso_Y2", "INLA"] <- JMinla$summary.hyperpar["Beta for eta2", "mean"]
      res["Asso_Y2", "INLA_sd"] <-  JMinla$summary.hyperpar["Beta for eta2", "sd"]
      res["Asso_Y2", "INLA_025"] <-  JMinla$summary.hyperpar["Beta for eta2", "0.025quant"]
      res["Asso_Y2", "INLA_975"] <-  JMinla$summary.hyperpar["Beta for eta2", "0.975quant"]
      res["Asso_Y3", "INLA"] <- JMinla$summary.hyperpar["Beta for eta3", "mean"]
      res["Asso_Y3", "INLA_sd"] <-  JMinla$summary.hyperpar["Beta for eta3", "sd"]
      res["Asso_Y3", "INLA_025"] <-  JMinla$summary.hyperpar["Beta for eta3", "0.025quant"]
      res["Asso_Y3", "INLA_975"] <-  JMinla$summary.hyperpar["Beta for eta3", "0.975quant"]
      res["Comp. time (sec.)", "INLA"] <- JMinla$cpu.used["Total"]
      res["Comp. time (sec.)", "INLA_sd"] <- difftime(b_INLA, a_INLA, units="secs")
      res["Comp. time (sec.)", "INLA_025"] <- 0
      res["Comp. time (sec.)", "INLA_975"] <- 0
      
      print(round(res[, c("True Val.","INLA", "INLA_sd", "INLA_025", "INLA_975")],2))
      res_list_INLA[[i]] <- res
      if(SAVE_res)  save(res_list_INLA, file="./res_list_INLA.RData")
    }}}
}


m=rowMeans(matrix(unlist(lapply(res_list_INLA, function(x) x[,"INLA"])), ncol=nmod))
ss=sqrt(rowMeans(matrix(unlist(lapply(res_list_INLA, function(x) (x[,"INLA_sd"])^2)), ncol=nmod)))
tru <- res_list_INLA[[1]][,1]
print(round(dig = 4, cbind(tru, m, ss, z.score = (tru-m)/ss/sqrt(nmod))))
























