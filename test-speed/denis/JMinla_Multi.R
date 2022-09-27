library(INLA)
INLA:::inla.my.update(b = T)
library(JM) # contains the dataset
inla.setOption(inla.mode="experimental")
inla.setOption(inla.call="remote")
inla.setOption(num.threads="100:1")
set.seed(1) # seed for data generation

# store results in object "res"
cols <- c("True Val.","INLA", "INLA_sd","INLA_025","INLA_975")
rows <- c("Y1_intercept", "Y1_slope", "Y1_ctsX", "Y1_binX", "Y1_err",
          "var_int1", "var_slo1",
          "cov_i1s1",
          "Asso_Y1",
          "Comp. time (sec.)"
)
res=matrix(data = NA, ncol=length(cols), nrow=length(rows))
colnames(res) <- cols
rownames(res) <- rows
JMinla <- NULL

data(pbc2)
#pbc2 <- pbc2[which(pbc2$id %in% c(1:5)),]
Longi <- na.omit(pbc2[, c("id", "years", "status","drug","age","sex","year","serBilir","SGOT", "albumin",
                          "platelets", "alkaline","spiders", "ascites")])# "prothrombin", "hepatomegaly"
Longi$id <- as.integer(Longi$id)
Longi$sex <- ifelse(Longi$sex=="male",1,0)
Longi$drug <- ifelse(Longi$drug=="D-penicil",1,0)
# age categories
quantile(Longi$age, 0.33)
quantile(Longi$age, 0.66)
Longi$age45_55 <- ifelse(Longi$age>=45 & Longi$age<55,1,0)
Longi$ageM55 <- ifelse(Longi$age>=55,1,0)
summary(Longi)
Surv <- Longi[,-c(7:16)]
Surv$id <- as.integer(Surv$id)
Surv$death <- ifelse(Longi$status=="dead",1,0)
Surv$trans <- ifelse(Longi$status=="transplanted",1,0)
Surv <- Surv[!duplicated(Surv),]

NL <- dim(Longi)[1] # number of longitudinal repeated measurements
NS <- dim(Surv)[1] # number of individuals / event times








YD <- inla.surv(time = c(Surv$years), event = c(Surv$death)) # Death event
YT <- inla.surv(time = c(Surv$years), event = c(Surv$trans)) # Transplantation event
NintBH=15
cox_death= inla.coxph(YD ~ -1+Int+drug,#+age45_55+ageM55+sex
                      control.hazard=list(model="rw2", scale.model=TRUE,
                                          diagonal=1e-2,constr=TRUE, n.intervals=NintBH,
                                          hyper=list(prec=list(prior="pc.prec", param=c(0.5,0.01)))),
                      data = c(list(YD = YD,
                                    Int = rep(1, NS),
                                    # age45_55=Surv$age45_55,
                                    # ageM55=Surv$ageM55,
                                    # sex=Surv$sex,
                                    drug=Surv$drug,
                                    CV_Y1 = NS*4+Surv$id,
                                    CS_Y1 = NS*5+Surv$id,
                                    CV_Y2 = NS*6+Surv$id,
                                    CV_Y3 = NS*7+Surv$id,
                                    CV_Y4 = NS*8+Surv$id,
                                    CV_Y5 = NS*9+Surv$id)))
cox_trans= inla.coxph(YT ~ -1+IntTR+drugTR,#+age45_55TR+ageM55TR+sexTR
                      control.hazard=list(model="rw1", scale.model=TRUE,
                                          diagonal=1e-2,constr=TRUE, n.intervals=NintBH,
                                          hyper=list(prec=list(prior="pc.prec", param=c(0.5,0.01)))),
                      data = c(list(YT = YT,
                                    IntTR = rep(1, NS),
                                    # age45_55TR=Surv$age45_55,
                                    # ageM55TR=Surv$ageM55,
                                    # sexTR=Surv$sex,
                                    drugTR=Surv$drug,
                                    CVTR_Y1 = NS*4+Surv$id,
                                    CVTR_Y3 = NS*7+Surv$id,
                                    CVTR_Y4 = NS*8+Surv$id,
                                    CVTR_Y5 = NS*9+Surv$id)), tag="TR")
# time weight for time-dependent covariates
re.weight <- cox_death$data$baseline.hazard.time +  0.5 * cox_death$data$baseline.hazard.length
ns_cox = dim(cox_death$data)[1]
IDCV <- cox_death$data$CV_Y1
cox_death$data$CV_Y1 <- NS*4+1:ns_cox # unique value for each survival interval for the associations (time-dependent)
cox_death$data$CS_Y1 <- NS*4+ns_cox+1:ns_cox
cox_death$data$CV_Y2 <- NS*4+ns_cox*2+1:ns_cox
cox_death$data$CV_Y3 <- NS*4+ns_cox*3+1:ns_cox
cox_death$data$CV_Y4 <- NS*4+ns_cox*4+1:ns_cox
cox_death$data$CV_Y5 <- NS*4+ns_cox*5+1:ns_cox
cox_trans$data$CVTR_Y1 <- NS*4+1:ns_cox
cox_trans$data$CVTR_Y3 <- NS*4+ns_cox*3+1:ns_cox
cox_trans$data$CVTR_Y4 <- NS*4+ns_cox*4+1:ns_cox
cox_trans$data$CVTR_Y5 <- NS*4+ns_cox*5+1:ns_cox









# quantiles for splines knots
quantile(pbc2$year,0.33) # 1 year
quantile(pbc2$year,0.66) # 4 year
# x <- seq(0,15,by=0.1)
# y <- ns(x, knots=c(1,4))
# plot(x, y[,1], type='l', ylim=c(-0.5,1))
# lines(x,y[,2], lty=2)
# lines(x,y[,3], lty=3)
Nsplines <- ns(Longi$year, knots=c(1,4))
NSweight <- ns(re.weight, knots=c(1,4))
DNSweight <- dns(re.weight, knots=c(1,4)) # derivative of natural spline (for current slope assoc)











# first marker: Bilirubin
InteY1 = c(rep(1,NL+ns_cox), rep(NA,ns_cox)) # intercept Y1
NS1Y1 = c(Nsplines[,1],NSweight[,1], DNSweight[,1]) # Nspline 1 Y1
NS2Y1 = c(Nsplines[,2],NSweight[,2], DNSweight[,2]) # Nspline 2 Y1
NS3Y1 = c(Nsplines[,3],NSweight[,3], DNSweight[,3]) # Nspline 3 Y1
drugY1 = c(Longi$drug, cox_death$data$drug, rep(NA,ns_cox)) # drug Y1
NS1drugY1 = c(Nsplines[,1]*Longi$drug,NSweight[,1]*cox_death$data$drug, DNSweight[,1]*cox_death$data$drug) # Nspline 1 Y1 * drug
NS2drugY1 = c(Nsplines[,2]*Longi$drug,NSweight[,2]*cox_death$data$drug, DNSweight[,2]*cox_death$data$drug) # Nspline 2 Y1 * drug
NS3drugY1 = c(Nsplines[,3]*Longi$drug,NSweight[,3]*cox_death$data$drug, DNSweight[,3]*cox_death$data$drug) # Nspline 3 Y1 * drug

IDY1=c(Longi$id, IDCV-NS*4, rep(NA,ns_cox)) # random intercept Y1
IDNS1Y1=c(NS+Longi$id, IDCV-NS*3, IDCV-NS*3) # random effect Nspline 1 Y1
WNS1Y1=c(Nsplines[,1],NSweight[,1], DNSweight[,1]) # weight
IDNS2Y1=c(NS*2+Longi$id, IDCV-NS*2, IDCV-NS*2) # random effect Nspline 2 Y1
WNS2Y1=c(Nsplines[,2],NSweight[,2], DNSweight[,2]) # weight
IDNS3Y1=c(NS*3+Longi$id, IDCV-NS, IDCV-NS) # random effect Nspline 3 Y1
WNS3Y1=c(Nsplines[,3],NSweight[,3], DNSweight[,3]) # weight
u1 = c(rep(NA, NL), cox_death$data$CV_Y1, rep(NA,ns_cox))
w1 = c(rep(NA, NL), rep(-1, ns_cox), rep(NA,ns_cox)) # weight random slope Y1
u1slope = c(rep(NA, NL+ns_cox), cox_death$data$CS_Y1)
w1slope = c(rep(NA, NL+ns_cox), rep(-1, ns_cox)) # Y1 weight association survival

Y1 <- c(Longi$serBilir, rep(NA, ns_cox*2)) # Y1 outcome
Y1.CV <- c(rep(NA, NL), rep(0, ns_cox), rep(NA, ns_cox)) # association Y1: current value
Y1.CS <- c(rep(NA, NL+ns_cox), rep(0, ns_cox)) # association Y1: current slope



# second marker: SGOT
InteY1 = c(InteY1, rep(NA, NL+ns_cox)) # intercept Y1
NS1Y1 = c(NS1Y1,rep(NA,NL+ns_cox)) # Nspline 1 Y1
NS2Y1 = c(NS2Y1,rep(NA,NL+ns_cox)) # Nspline 2 Y1
NS3Y1 = c(NS3Y1,rep(NA,NL+ns_cox)) # Nspline 3 Y1
drugY1 = c(drugY1,rep(NA,NL+ns_cox)) # drug Y1
NS1drugY1 = c(NS1drugY1,rep(NA,NL+ns_cox)) # Nspline 1 Y1 * drug
NS2drugY1 = c(NS2drugY1,rep(NA,NL+ns_cox)) # Nspline 2 Y1 * drug
NS3drugY1 = c(NS3drugY1,rep(NA,NL+ns_cox)) # Nspline 3 Y1 * drug
InteY2 = c(rep(NA,NL+ns_cox*2), rep(1,NL+ns_cox)) # intercept Y2
NS1Y2 = c(rep(NA,NL+ns_cox*2), Nsplines[,1], NSweight[,1]) # Nspline 1 Y2
NS2Y2 = c(rep(NA,NL+ns_cox*2), Nsplines[,2], NSweight[,2]) # Nspline 2 Y2
NS3Y2 = c(rep(NA,NL+ns_cox*2), Nsplines[,3], NSweight[,3]) # Nspline 3 Y2
drugY2 = c(rep(NA,NL+ns_cox*2), Longi$drug, cox_death$data$drug) # drug Y2
NS1drugY2 = c(rep(NA,NL+ns_cox*2), Nsplines[,1]*Longi$drug, NSweight[,1]*cox_death$data$drug) # Nspline 1 Y2 * drug
NS2drugY2 = c(rep(NA,NL+ns_cox*2), Nsplines[,2]*Longi$drug, NSweight[,2]*cox_death$data$drug) # Nspline 2 Y2 * drug
NS3drugY2 = c(rep(NA,NL+ns_cox*2), Nsplines[,3]*Longi$drug, NSweight[,3]*cox_death$data$drug) # Nspline 3 Y2 * drug

IDY1=c(IDY1, rep(NA,NL+ns_cox)) # random intercept Y1
IDNS1Y1=c(IDNS1Y1, rep(NA,NL+ns_cox)) # random effect Nspline 1 Y1
WNS1Y1=c(WNS1Y1, rep(NA,NL+ns_cox)) # weight
IDNS2Y1=c(IDNS2Y1, rep(NA,NL+ns_cox)) # random effect Nspline 2 Y1
WNS2Y1=c(WNS2Y1, rep(NA,NL+ns_cox)) # weight
IDNS3Y1=c(IDNS3Y1, rep(NA,NL+ns_cox)) # random effect Nspline 3 Y1
WNS3Y1=c(WNS3Y1, rep(NA,NL+ns_cox)) # weight
u1 = c(u1, rep(NA,NL+ns_cox))
w1 = c(w1, rep(NA,NL+ns_cox)) # weight random slope Y1
u1slope = c(u1slope, rep(NA,NL+ns_cox))
w1slope = c(w1slope, rep(NA,NL+ns_cox))
IDY2=c(rep(NA,NL+ns_cox*2), Longi$id, IDCV-NS*4) # random intercept Y2
IDNS1Y2=c(rep(NA,NL+ns_cox*2), NS+Longi$id, IDCV-NS*3) # random effect Nspline 1 Y2
WNS1Y2=c(rep(NA,NL+ns_cox*2), Nsplines[,1],NSweight[,1]) # weight
IDNS2Y2=c(rep(NA,NL+ns_cox*2), NS*2+Longi$id, IDCV-NS*2) # random effect Nspline 2 Y2
WNS2Y2=c(rep(NA,NL+ns_cox*2), Nsplines[,2],NSweight[,2]) # weight
IDNS3Y2=c(rep(NA,NL+ns_cox*2), NS*3+Longi$id, IDCV-NS) # random effect Nspline 3 Y2
WNS3Y2=c(rep(NA,NL+ns_cox*2), Nsplines[,3],NSweight[,3]) # weight
u2 = c(rep(NA,NL+ns_cox*2), rep(NA, NL), cox_death$data$CV_Y2)
w2 = c(rep(NA,NL+ns_cox*2), rep(NA, NL), rep(-1, ns_cox)) # Y2 weight association survival

Y1 <- c(Y1, rep(NA, NL+ns_cox)) # Y1 outcome
Y2 <- c(rep(NA, NL+ns_cox*2), Longi$SGOT, rep(NA, ns_cox)) # Y2 outcome
Y1.CV <- c(Y1.CV, rep(NA, NL+ns_cox)) # association Y1: current value
Y1.CS <- c(Y1.CS, rep(NA, NL+ns_cox)) # association Y1: current slope
Y2.CV <- c(rep(NA, NL*2+ns_cox*2), rep(0, ns_cox)) # association Y2: current value



# third marker: albumin
InteY1 = c(InteY1, rep(NA, NL+ns_cox)) # intercept Y1
NS1Y1 = c(NS1Y1,rep(NA,NL+ns_cox)) # Nspline 1 Y1
NS2Y1 = c(NS2Y1,rep(NA,NL+ns_cox)) # Nspline 2 Y1
NS3Y1 = c(NS3Y1,rep(NA,NL+ns_cox)) # Nspline 3 Y1
drugY1 = c(drugY1,rep(NA,NL+ns_cox)) # drug Y1
NS1drugY1 = c(NS1drugY1,rep(NA,NL+ns_cox)) # Nspline 1 Y1 * drug
NS2drugY1 = c(NS2drugY1,rep(NA,NL+ns_cox)) # Nspline 2 Y1 * drug
NS3drugY1 = c(NS3drugY1,rep(NA,NL+ns_cox)) # Nspline 3 Y1 * drug
InteY2 = c(InteY2,rep(NA,NL+ns_cox)) # intercept Y2
NS1Y2 = c(NS1Y2,rep(NA,NL+ns_cox)) # Nspline 1 Y2
NS2Y2 = c(NS2Y2,rep(NA,NL+ns_cox)) # Nspline 2 Y2
NS3Y2 = c(NS3Y2,rep(NA,NL+ns_cox)) # Nspline 3 Y2
drugY2 = c(drugY2,rep(NA,NL+ns_cox)) # drug Y2
NS1drugY2 = c(NS1drugY2,rep(NA,NL+ns_cox)) # Nspline 1 Y2 * drug
NS2drugY2 = c(NS2drugY2,rep(NA,NL+ns_cox)) # Nspline 2 Y2 * drug
NS3drugY2 = c(NS3drugY2,rep(NA,NL+ns_cox)) # Nspline 3 Y2 * drug
InteY3 = c(rep(NA,NL*2+ns_cox*3), rep(1,NL+ns_cox)) # intercept Y2
TIMEY3 = c(rep(NA,NL*2+ns_cox*3), Longi$year, re.weight) # time Y3
drugY3 = c(rep(NA,NL*2+ns_cox*3), Longi$drug, cox_death$data$drug) # drug Y2
TIMEdrugY3 = c(rep(NA,NL*2+ns_cox*3), Longi$year*Longi$drug, re.weight*cox_death$data$drug) # time Y3 * drug

IDY1=c(IDY1, rep(NA,NL+ns_cox)) # random intercept Y1
IDNS1Y1=c(IDNS1Y1, rep(NA,NL+ns_cox)) # random effect Nspline 1 Y1
WNS1Y1=c(WNS1Y1, rep(NA,NL+ns_cox)) # weight
IDNS2Y1=c(IDNS2Y1, rep(NA,NL+ns_cox)) # random effect Nspline 2 Y1
WNS2Y1=c(WNS2Y1, rep(NA,NL+ns_cox)) # weight
IDNS3Y1=c(IDNS3Y1, rep(NA,NL+ns_cox)) # random effect Nspline 3 Y1
WNS3Y1=c(WNS3Y1, rep(NA,NL+ns_cox)) # weight
u1 = c(u1, rep(NA,NL+ns_cox))
w1 = c(w1, rep(NA,NL+ns_cox)) # weight random slope Y1
u1slope = c(u1slope, rep(NA,NL+ns_cox))
w1slope = c(w1slope, rep(NA,NL+ns_cox))
IDY2=c(IDY2, rep(NA,NL+ns_cox)) # random intercept Y2
IDNS1Y2=c(IDNS1Y2, rep(NA,NL+ns_cox)) # random effect Nspline 1 Y2
WNS1Y2=c(WNS1Y2, rep(NA,NL+ns_cox)) # weight
IDNS2Y2=c(IDNS2Y2, rep(NA,NL+ns_cox)) # random effect Nspline 2 Y2
WNS2Y2=c(WNS2Y2, rep(NA,NL+ns_cox)) # weight
IDNS3Y2=c(IDNS3Y2, rep(NA,NL+ns_cox)) # random effect Nspline 3 Y2
WNS3Y2=c(WNS3Y2, rep(NA,NL+ns_cox)) # weight
u2 = c(u2, rep(NA,NL+ns_cox))
w2 = c(w2, rep(NA,NL+ns_cox)) # Y2 weight association survival
IDY3=c(rep(NA,NL*2+ns_cox*3), Longi$id, IDCV-NS*4) # random intercept Y3
IDTIMEY3=c(rep(NA,NL*2+ns_cox*3), NS+Longi$id, IDCV-NS*3) # random effect TIME Y3
WTIMEY3=c(rep(NA,NL*2+ns_cox*3), Longi$year, re.weight) # weight
u3 = c(rep(NA,NL*3+ns_cox*3), cox_death$data$CV_Y3)
w3 = c(rep(NA,NL*3+ns_cox*3), rep(-1, ns_cox)) # weight random slope Y3

Y1 <- c(Y1, rep(NA, NL+ns_cox)) # Y1 outcome
Y2 <- c(Y2, rep(NA, NL+ns_cox)) # Y2 outcome
Y3 <- c(rep(NA, NL*2+ns_cox*3), Longi$albumin, rep(NA, ns_cox)) # Y3 outcome
Y1.CV <- c(Y1.CV, rep(NA, NL+ns_cox)) # association Y1: current value
Y1.CS <- c(Y1.CS, rep(NA, NL+ns_cox)) # association Y1: current slope
Y2.CV <- c(Y2.CV, rep(NA, NL+ns_cox)) # association Y2: current value
Y3.CV <- c(rep(NA, NL*3+ns_cox*3),  rep(0, ns_cox))# association Y3: current value





# fourth marker: platelets
InteY1 = c(InteY1, rep(NA, NL+ns_cox)) # intercept Y1
NS1Y1 = c(NS1Y1,rep(NA,NL+ns_cox)) # Nspline 1 Y1
NS2Y1 = c(NS2Y1,rep(NA,NL+ns_cox)) # Nspline 2 Y1
NS3Y1 = c(NS3Y1,rep(NA,NL+ns_cox)) # Nspline 3 Y1
drugY1 = c(drugY1,rep(NA,NL+ns_cox)) # drug Y1
NS1drugY1 = c(NS1drugY1,rep(NA,NL+ns_cox)) # Nspline 1 Y1 * drug
NS2drugY1 = c(NS2drugY1,rep(NA,NL+ns_cox)) # Nspline 2 Y1 * drug
NS3drugY1 = c(NS3drugY1,rep(NA,NL+ns_cox)) # Nspline 3 Y1 * drug
InteY2 = c(InteY2,rep(NA,NL+ns_cox)) # intercept Y2
NS1Y2 = c(NS1Y2,rep(NA,NL+ns_cox)) # Nspline 1 Y2
NS2Y2 = c(NS2Y2,rep(NA,NL+ns_cox)) # Nspline 2 Y2
NS3Y2 = c(NS3Y2,rep(NA,NL+ns_cox)) # Nspline 3 Y2
drugY2 = c(drugY2,rep(NA,NL+ns_cox)) # drug Y2
NS1drugY2 = c(NS1drugY2,rep(NA,NL+ns_cox)) # Nspline 1 Y2 * drug
NS2drugY2 = c(NS2drugY2,rep(NA,NL+ns_cox)) # Nspline 2 Y2 * drug
NS3drugY2 = c(NS3drugY2,rep(NA,NL+ns_cox)) # Nspline 3 Y2 * drug
InteY3 = c(InteY3,rep(NA,NL+ns_cox)) # intercept Y2
TIMEY3 = c(TIMEY3,rep(NA,NL+ns_cox)) # time Y3
drugY3 = c(drugY3,rep(NA,NL+ns_cox)) # drug Y2
TIMEdrugY3 = c(TIMEdrugY3,rep(NA,NL+ns_cox)) # time Y3 * drug
InteY4 = c(rep(NA,NL*3+ns_cox*4), rep(1,NL+ns_cox)) # intercept Y4
NS1Y4 = c(rep(NA,NL*3+ns_cox*4), Nsplines[,1], NSweight[,1]) # Nspline 1 Y4
NS2Y4 = c(rep(NA,NL*3+ns_cox*4), Nsplines[,2], NSweight[,2]) # Nspline 2 Y4
NS3Y4 = c(rep(NA,NL*3+ns_cox*4), Nsplines[,3], NSweight[,3]) # Nspline 3 Y4
drugY4 = c(rep(NA,NL*3+ns_cox*4), Longi$drug, cox_death$data$drug) # drug Y4
NS1drugY4 = c(rep(NA,NL*3+ns_cox*4), Nsplines[,1]*Longi$drug, NSweight[,1]*cox_death$data$drug) # Nspline 1 Y4 * drug
NS2drugY4 = c(rep(NA,NL*3+ns_cox*4), Nsplines[,2]*Longi$drug, NSweight[,2]*cox_death$data$drug) # Nspline 2 Y4 * drug
NS3drugY4 = c(rep(NA,NL*3+ns_cox*4), Nsplines[,3]*Longi$drug, NSweight[,3]*cox_death$data$drug) # Nspline 3 Y4 * drug

IDY1=c(IDY1, rep(NA,NL+ns_cox)) # random intercept Y1
IDNS1Y1=c(IDNS1Y1, rep(NA,NL+ns_cox)) # random effect Nspline 1 Y1
WNS1Y1=c(WNS1Y1, rep(NA,NL+ns_cox)) # weight
IDNS2Y1=c(IDNS2Y1, rep(NA,NL+ns_cox)) # random effect Nspline 2 Y1
WNS2Y1=c(WNS2Y1, rep(NA,NL+ns_cox)) # weight
IDNS3Y1=c(IDNS3Y1, rep(NA,NL+ns_cox)) # random effect Nspline 3 Y1
WNS3Y1=c(WNS3Y1, rep(NA,NL+ns_cox)) # weight
u1 = c(u1, rep(NA,NL+ns_cox))
w1 = c(w1, rep(NA,NL+ns_cox)) # weight random slope Y1
u1slope = c(u1slope, rep(NA,NL+ns_cox))
w1slope = c(w1slope, rep(NA,NL+ns_cox))
IDY2=c(IDY2, rep(NA,NL+ns_cox)) # random intercept Y2
IDNS1Y2=c(IDNS1Y2, rep(NA,NL+ns_cox)) # random effect Nspline 1 Y2
WNS1Y2=c(WNS1Y2, rep(NA,NL+ns_cox)) # weight
IDNS2Y2=c(IDNS2Y2, rep(NA,NL+ns_cox)) # random effect Nspline 2 Y2
WNS2Y2=c(WNS2Y2, rep(NA,NL+ns_cox)) # weight
IDNS3Y2=c(IDNS3Y2, rep(NA,NL+ns_cox)) # random effect Nspline 3 Y2
WNS3Y2=c(WNS3Y2, rep(NA,NL+ns_cox)) # weight
u2 = c(u2, rep(NA,NL+ns_cox))
w2 = c(w2, rep(NA,NL+ns_cox)) # Y2 weight association survival
IDY3=c(IDY3, rep(NA,NL+ns_cox)) # random intercept Y3
IDTIMEY3=c(IDTIMEY3, rep(NA,NL+ns_cox)) # random effect TIME Y3
WTIMEY3=c(WTIMEY3, rep(NA,NL+ns_cox)) # weight
u3 = c(u3, rep(NA,NL+ns_cox))
w3 = c(w3, rep(NA,NL+ns_cox)) # weight random slope Y3
IDY4=c(rep(NA,NL*3+ns_cox*4), Longi$id, IDCV-NS*4) # random intercept Y4
IDNS1Y4=c(rep(NA,NL*3+ns_cox*4), NS+Longi$id, IDCV-NS*3) # random effect Nspline 1 Y4
WNS1Y4=c(rep(NA,NL*3+ns_cox*4), Nsplines[,1],NSweight[,1]) # weight
IDNS2Y4=c(rep(NA,NL*3+ns_cox*4), NS*2+Longi$id, IDCV-NS*2) # random effect Nspline 2 Y4
WNS2Y4=c(rep(NA,NL*3+ns_cox*4), Nsplines[,2],NSweight[,2]) # weight
IDNS3Y4=c(rep(NA,NL*3+ns_cox*4), NS*3+Longi$id, IDCV-NS) # random effect Nspline 3 Y4
WNS3Y4=c(rep(NA,NL*3+ns_cox*4), Nsplines[,3],NSweight[,3]) # weight
u4 = c(rep(NA,NL*3+ns_cox*4), rep(NA, NL), cox_death$data$CV_Y4)
w4 = c(rep(NA,NL*3+ns_cox*4), rep(NA, NL), rep(-1, ns_cox)) # Y4 weight association survival

Y1 <- c(Y1, rep(NA, NL+ns_cox)) # Y1 outcome
Y2 <- c(Y2, rep(NA, NL+ns_cox)) # Y2 outcome
Y3 <- c(Y3, rep(NA, NL+ns_cox)) # Y3 outcome
Y4 <- c(rep(NA, NL*3+ns_cox*4), Longi$platelets, rep(NA, ns_cox)) # Y4 outcome
Y1.CV <- c(Y1.CV, rep(NA, NL+ns_cox)) # association Y1: current value
Y1.CS <- c(Y1.CS, rep(NA, NL+ns_cox)) # association Y1: current slope
Y2.CV <- c(Y2.CV, rep(NA, NL+ns_cox)) # association Y2: current value
Y3.CV <- c(Y3.CV, rep(NA, NL+ns_cox))# association Y3: current value
Y4.CV <- c(rep(NA, NL*4+ns_cox*4),  rep(0, ns_cox))# association Y4: current value




# fifth marker: spiders
linear.covariate <- list(
InteY1 = c(InteY1, rep(NA, NL+ns_cox)), # intercept Y1
NS1Y1 = c(NS1Y1,rep(NA,NL+ns_cox)), # Nspline 1 Y1
NS2Y1 = c(NS2Y1,rep(NA,NL+ns_cox)), # Nspline 2 Y1
NS3Y1 = c(NS3Y1,rep(NA,NL+ns_cox)), # Nspline 3 Y1
drugY1 = c(drugY1,rep(NA,NL+ns_cox)), # drug Y1
NS1drugY1 = c(NS1drugY1,rep(NA,NL+ns_cox)), # Nspline 1 Y1 * drug
NS2drugY1 = c(NS2drugY1,rep(NA,NL+ns_cox)), # Nspline 2 Y1 * drug
NS3drugY1 = c(NS3drugY1,rep(NA,NL+ns_cox)), # Nspline 3 Y1 * drug
InteY2 = c(InteY2,rep(NA,NL+ns_cox)), # intercept Y2
NS1Y2 = c(NS1Y2,rep(NA,NL+ns_cox)), # Nspline 1 Y2
NS2Y2 = c(NS2Y2,rep(NA,NL+ns_cox)), # Nspline 2 Y2
NS3Y2 = c(NS3Y2,rep(NA,NL+ns_cox)), # Nspline 3 Y2
drugY2 = c(drugY2,rep(NA,NL+ns_cox)), # drug Y2
NS1drugY2 = c(NS1drugY2,rep(NA,NL+ns_cox)), # Nspline 1 Y2 * drug
NS2drugY2 = c(NS2drugY2,rep(NA,NL+ns_cox)), # Nspline 2 Y2 * drug
NS3drugY2 = c(NS3drugY2,rep(NA,NL+ns_cox)), # Nspline 3 Y2 * drug
InteY3 = c(InteY3,rep(NA,NL+ns_cox)), # intercept Y2
TIMEY3 = c(TIMEY3,rep(NA,NL+ns_cox)), # time Y3
drugY3 = c(drugY3,rep(NA,NL+ns_cox)), # drug Y2
TIMEdrugY3 = c(TIMEdrugY3,rep(NA,NL+ns_cox)), # time Y3 * drug
InteY4 = c(InteY4,rep(NA,NL+ns_cox)), # intercept Y4
NS1Y4 = c(NS1Y4,rep(NA,NL+ns_cox)), # Nspline 1 Y4
NS2Y4 = c(NS2Y4,rep(NA,NL+ns_cox)), # Nspline 2 Y4
NS3Y4 = c(NS3Y4,rep(NA,NL+ns_cox)), # Nspline 3 Y4
drugY4 = c(drugY4,rep(NA,NL+ns_cox)), # drug Y4
NS1drugY4 = c(NS1drugY4,rep(NA,NL+ns_cox)), # Nspline 1 Y4 * drug
NS2drugY4 = c(NS2drugY4,rep(NA,NL+ns_cox)), # Nspline 2 Y4 * drug
NS3drugY4 = c(NS3drugY4,rep(NA,NL+ns_cox)), # Nspline 3 Y4 * drug
InteY5 = c(rep(NA,NL*4+ns_cox*5), rep(1,NL+ns_cox)), # intercept Y5
TIMEY5 = c(rep(NA,NL*4+ns_cox*5), Longi$year, re.weight), # time Y5
drugY5 = c(rep(NA,NL*4+ns_cox*5), Longi$drug, cox_death$data$drug), # drug Y5
TIMEdrugY5 = c(rep(NA,NL*4+ns_cox*5), Longi$year*Longi$drug, re.weight*cox_death$data$drug) # time Y5 * drug
)
random.covariate <- list(
IDY1=c(IDY1, rep(NA,NL+ns_cox)), # random intercept Y1
IDNS1Y1=c(IDNS1Y1, rep(NA,NL+ns_cox)), # random effect Nspline 1 Y1
WNS1Y1=c(WNS1Y1, rep(NA,NL+ns_cox)), # weight
IDNS2Y1=c(IDNS2Y1, rep(NA,NL+ns_cox)), # random effect Nspline 2 Y1
WNS2Y1=c(WNS2Y1, rep(NA,NL+ns_cox)), # weight
IDNS3Y1=c(IDNS3Y1, rep(NA,NL+ns_cox)), # random effect Nspline 3 Y1
WNS3Y1=c(WNS3Y1, rep(NA,NL+ns_cox)), # weight
u1 = c(u1, rep(NA,NL+ns_cox)),
w1 = c(w1, rep(NA,NL+ns_cox)), # weight random slope Y1
u1slope = c(u1slope, rep(NA,NL+ns_cox)),
w1slope = c(w1slope, rep(NA,NL+ns_cox)),
IDY2=c(IDY2, rep(NA,NL+ns_cox)), # random intercept Y2
IDNS1Y2=c(IDNS1Y2, rep(NA,NL+ns_cox)), # random effect Nspline 1 Y2
WNS1Y2=c(WNS1Y2, rep(NA,NL+ns_cox)), # weight
IDNS2Y2=c(IDNS2Y2, rep(NA,NL+ns_cox)), # random effect Nspline 2 Y2
WNS2Y2=c(WNS2Y2, rep(NA,NL+ns_cox)), # weight
IDNS3Y2=c(IDNS3Y2, rep(NA,NL+ns_cox)), # random effect Nspline 3 Y2
WNS3Y2=c(WNS3Y2, rep(NA,NL+ns_cox)), # weight
u2 = c(u2, rep(NA,NL+ns_cox)),
w2 = c(w2, rep(NA,NL+ns_cox)), # Y2 weight association survival
IDY3=c(IDY3, rep(NA,NL+ns_cox)), # random intercept Y3
IDTIMEY3=c(IDTIMEY3, rep(NA,NL+ns_cox)), # random effect TIME Y3
WTIMEY3=c(WTIMEY3, rep(NA,NL+ns_cox)), # weight
u3 = c(u3, rep(NA,NL+ns_cox)),
w3 = c(w3, rep(NA,NL+ns_cox)), # weight random slope Y3
IDY4=c(IDY4, rep(NA,NL+ns_cox)), # random intercept Y4
IDNS1Y4=c(IDNS1Y4, rep(NA,NL+ns_cox)), # random effect Nspline 1 Y4
WNS1Y4=c(WNS1Y4, rep(NA,NL+ns_cox)), # weight
IDNS2Y4=c(IDNS2Y4, rep(NA,NL+ns_cox)), # random effect Nspline 2 Y4
WNS2Y4=c(WNS2Y4, rep(NA,NL+ns_cox)), # weight
IDNS3Y4=c(IDNS3Y4, rep(NA,NL+ns_cox)), # random effect Nspline 3 Y4
WNS3Y4=c(WNS3Y4, rep(NA,NL+ns_cox)), # weight
u4 = c(u4, rep(NA,NL+ns_cox)),
w4 = c(w4, rep(NA,NL+ns_cox)), # Y4 weight association survival
IDY5=c(rep(NA,NL*4+ns_cox*5), Longi$id, IDCV-NS*4), # random intercept Y5
IDTIMEY5=c(rep(NA,NL*4+ns_cox*5), NS+Longi$id, IDCV-NS*3), # random effect TIME Y5
WTIMEY5=c(rep(NA,NL*4+ns_cox*5), Longi$year, re.weight), # weight
u5 = c(rep(NA,NL*4+ns_cox*5), rep(NA, NL), cox_death$data$CV_Y5),
w5 = c(rep(NA,NL*4+ns_cox*5), rep(NA, NL), rep(-1, ns_cox)) # Y5 weight association survival
)
Y1 <- c(Y1, rep(NA, NL+ns_cox)) # Y1 outcome
Y2 <- c(Y2, rep(NA, NL+ns_cox)) # Y2 outcome
Y3 <- c(Y3, rep(NA, NL+ns_cox)) # Y3 outcome
Y4 <- c(Y4, rep(NA, NL+ns_cox)) # Y4 outcome
Y5 <- c(rep(NA, NL*4+ns_cox*5), as.integer(Longi$spiders)-1, rep(NA, ns_cox)) # Y5 outcome
Y1.CV <- c(Y1.CV, rep(NA, NL+ns_cox)) # association Y1: current value
Y1.CS <- c(Y1.CS, rep(NA, NL+ns_cox)) # association Y1: current slope
Y2.CV <- c(Y2.CV, rep(NA, NL+ns_cox)) # association Y2: current value
Y3.CV <- c(Y3.CV, rep(NA, NL+ns_cox))# association Y3: current value
Y4.CV <- c(Y4.CV, rep(NA, NL+ns_cox))# association Y4: current value
Y5.CV <- c(rep(NA, NL*5+ns_cox*5),  rep(0, ns_cox))# association Y5: current value


jointdf = data.frame(linear.covariate, random.covariate, Y1, Y1.CV, Y1.CS, Y2, Y2.CV, Y3, Y3.CV, Y4, Y4.CV, Y5, Y5.CV)
joint.data_cox <- c(as.list(inla.rbind.data.frames(jointdf, cox_death$data, cox_trans$data)),
                    cox_death$data.list,cox_trans$data.list)


Yjoint = cbind(joint.data_cox$Y1, joint.data_cox$Y1.CV, joint.data_cox$Y1.CS, joint.data_cox$Y2, joint.data_cox$Y2.CV,
               joint.data_cox$Y3, joint.data_cox$Y3.CV, joint.data_cox$Y4, joint.data_cox$Y4.CV, joint.data_cox$Y5, joint.data_cox$Y5.CV,
               joint.data_cox$y..coxph, joint.data_cox$yTR..coxph) # outcomes (longi and survival)
joint.data_cox$Y <- Yjoint


formulaSurv= update(cox_death$formula, Yjoint ~ . + IntTR + drugTR +#+ age45_55TR + ageM55TR + sexTR
                      f(baselineTR.hazard, model = "rw1", values = baselineTR.hazard.values,
                        hyper = list(prec = list(hyperid = 55001, name = "log precision",
                                                  short.name = "prec", initial = 4, fixed = FALSE, prior = "pc.prec",
                                                 param = c(0.5, 0.01), to.theta = function(x) log(x), from.theta = function(x) exp(x))),
                        constr = TRUE, diagonal = 0.01, scale.model = TRUE))

formulaJ= update(formulaSurv, Yjoint ~ . -1 + InteY1 + drugY1 + NS1Y1 + NS2Y1 + NS3Y1 + NS1drugY1 + NS1drugY1 + NS2drugY1 + NS3drugY1 +
                   InteY2 + drugY2 + NS1Y2 + NS2Y2 + NS3Y2 + NS1drugY2 + NS2drugY2 + NS3drugY2 +
                   InteY3 + drugY3 + TIMEY3 + TIMEdrugY3 +
                   InteY4 + drugY4 + NS1Y4 + NS2Y4 + NS3Y4 + NS1drugY4 + NS2drugY4 + NS3drugY4 +
                   InteY5 + drugY5 + TIMEY5 + TIMEdrugY5 +
                   f(IDY1, model = "iidkd", order = 4, n = NS * 4, constr = F, hyper = list(theta1 = list(param = c(10, 1, 1, 1,1,0,0,0,0,0,0)))) +
                   f(IDNS1Y1, WNS1Y1, copy = "IDY1") +
                   f(IDNS2Y1, WNS2Y1, copy = "IDY1") +
                   f(IDNS3Y1, WNS3Y1, copy = "IDY1") +
                   f(u1,w1, model = "iid", hyper = list(prec = list(initial = -6,fixed = TRUE)), constr = F) +
                   f(CV_Y1, copy = "u1", hyper = list(beta = list(fixed = FALSE,param = c(0, 1), initial = 0.1)))+
                   f(CVTR_Y1, copy = "u1", hyper = list(beta = list(fixed = FALSE,param = c(0, 1), initial = 0.1)))+
                   f(u1slope,w1slope, model = "iid", hyper = list(prec = list(initial = -6,fixed = TRUE)), constr = F) +
                   f(CS_Y1, copy = "u1slope", hyper = list(beta = list(fixed = FALSE,param = c(0, 1), initial = 0.1))) +
                   f(IDY2, model = "iidkd", order = 4, n = NS * 4, constr = F, hyper = list(theta1 = list(param = c(10, 1, 1, 1,1,0,0,0,0,0,0)))) +
                   f(IDNS1Y2, WNS1Y2, copy = "IDY2") +
                   f(IDNS2Y2, WNS2Y2, copy = "IDY2") +
                   f(IDNS3Y2, WNS3Y2, copy = "IDY2") +
                   f(u2,w2, model = "iid", hyper = list(prec = list(initial = -6,fixed = TRUE)), constr = F) +
                   f(CV_Y2, copy = "u2", hyper = list(beta = list(fixed = FALSE,param = c(0, 1), initial = 0.1))) +
                   f(IDY3, model = "iidkd", order = 2, n = NS * 2, constr = F, hyper = list(theta1 = list(param = c(10, 1, 1, 0)))) +
                   f(IDTIMEY3, WTIMEY3, copy = "IDY3") +
                   f(u3,w3, model = "iid", hyper = list(prec = list(initial = -6,fixed = TRUE)), constr = F) +
                   f(CV_Y3, copy = "u3", hyper = list(beta = list(fixed = FALSE,param = c(0, 1), initial = 0.1))) +
                   f(CVTR_Y3, copy = "u3", hyper = list(beta = list(fixed = FALSE,param = c(0, 1), initial = 0.1))) +
                   f(IDY4, model = "iidkd", order = 4, n = NS * 4, constr = F, hyper = list(theta1 = list(param = c(10, 1, 1, 1,1,0,0,0,0,0,0)))) +
                   f(IDNS1Y4, WNS1Y4, copy = "IDY4") +
                   f(IDNS2Y4, WNS2Y4, copy = "IDY4") +
                   f(IDNS3Y4, WNS3Y4, copy = "IDY4") +
                   f(u4,w4, model = "iid", hyper = list(prec = list(initial = -6,fixed = TRUE)), constr = F) +
                   f(CV_Y4, copy = "u4", hyper = list(beta = list(fixed = FALSE,param = c(0, 1), initial = 0.1)))+
                   f(CVTR_Y4, copy = "u4", hyper = list(beta = list(fixed = FALSE,param = c(0, 1), initial = 0.1)))+
                   f(IDY5, model = "iidkd", order = 2, n = NS * 2, constr = F, hyper = list(theta1 = list(param = c(10, 1, 1, 0)))) +
                   f(IDTIMEY5, WTIMEY5, copy = "IDY5") +
                   f(u5,w5, model = "iid", hyper = list(prec = list(initial = -6,fixed = TRUE)), constr = F) +
                   f(CV_Y5, copy = "u5", hyper = list(beta = list(fixed = FALSE,param = c(0, 1), initial = 0.1))))#+
                   #f(CVTR_Y5, copy = "u5", hyper = list(beta = list(fixed = FALSE,param = c(0, 1), initial = 0.1))))




joint.data_cox$E..coxph[(NL*3+ns_cox*4+1):(NL*3+ns_cox*4+NL)] <- 1 #poisson










 JMinla_Multi <- try(inla(formulaJ,family = c("lognormal", "gaussian", "gaussian", # families marker 1, current value, current slope
                                              "lognormal", "gaussian", # families marker 2, current value
                                              "gaussian", "gaussian", # families marker 3, current value
                                              "poisson", "gaussian", # families marker 4, current value
                                              "binomial", "gaussian", # families marker 5, current value
                                              cox_death$family, cox_trans$family), # cox families for risk of death and risk of transplantation
                         data=joint.data_cox,
                         control.fixed = list(mean=0, prec=1, mean.intercept=0, prec.intercept=1),
                         control.family = list(
                           list(),
                           list(hyper = list(prec = list(initial = 12, fixed=TRUE))),
                           list(hyper = list(prec = list(initial = 12, fixed=TRUE))),
                           list(),
                           list(hyper = list(prec = list(initial = 12, fixed=TRUE))),
                           list(),
                           list(hyper = list(prec = list(initial = 12, fixed=TRUE))),
                           list(),
                           list(hyper = list(prec = list(initial = 12, fixed=TRUE))),
                           list(),
                           list(hyper = list(prec = list(initial = 12, fixed=TRUE))),
                           list(),
                           list()
                         ),
                         control.compute=list(config = TRUE, dic=TRUE,cpo=TRUE,waic=TRUE),
                         E = joint.data_cox$E..coxph,
                         control.inla = list(int.strategy="eb",control.vb = list(f.enable.limit = 50), cmin = 0),#parallel.linesearch=T,
                         verbose=T))
 if(class(JMinla_Multi)=="inla") {
  setwd("~/Documents/PROJECTS/MULTI_LONGI/Application/")
    nrerun=0
   ## this indicate negative eigenvalue in Hessian
   while(nrerun<=5){
    while(("Inf" %in% JMinla_Multi$summary.hyperpar[-which(rownames(JMinla_Multi$summary.hyperpar)=="Precision for baseline.hazard"), "mean"] |
          "Inf" %in% JMinla_Multi$summary.hyperpar[-which(rownames(JMinla_Multi$summary.hyperpar)=="Precision for baselineTR.hazard"), "mean"]|
          JMinla_Multi$waic$waic>10^10 | JMinla_Multi$summary.hyperpar["Precision for baselineTR.hazard","sd"]>10^6)){
   #while (sum(abs(JMinla_Multi$misc$cov.intern[upper.tri(JMinla_Multi$misc$cov.intern)])) == 0 & nrerun<=5){
       print(summary(JMinla_Multi))
     print(paste(" *** RERUN: ", nrerun))
     JMinla_Multi <-  try(inla.rerun(JMinla_Multi)) # loop
   }
          nrerun=nrerun+1
 print(summary(JMinla_Multi))
  }
 }
 # save(JMinla_Multi, file="./JMinla_Multi.RData")

  # Get random effects covariance from Cholesky
  MC_samplesIDY1 <- inla.iidkd.sample(10^4, JMinla_Multi, "IDY1", return.cov=TRUE)
  MC_samplesIDY2 <- inla.iidkd.sample(10^4, JMinla_Multi, "IDY2", return.cov=TRUE)
  MC_samplesIDY3 <- inla.iidkd.sample(10^4, JMinla_Multi, "IDY3", return.cov=TRUE)
  MC_samplesIDY4 <- inla.iidkd.sample(10^4, JMinla_Multi, "IDY4", return.cov=TRUE)
  MC_samplesIDY5 <- inla.iidkd.sample(10^4, JMinla_Multi, "IDY5", return.cov=TRUE)
  VarCovIDY1 <- matrix(unlist(MC_samplesIDY1), nrow = 4^2)
  VarCovIDY2 <- matrix(unlist(MC_samplesIDY2), nrow = 4^2)
  VarCovIDY3 <- matrix(unlist(MC_samplesIDY3), nrow = 2^2)
  VarCovIDY4 <- matrix(unlist(MC_samplesIDY4), nrow = 4^2)
  VarCovIDY5 <- matrix(unlist(MC_samplesIDY5), nrow = 2^2)
  VarCovMeansIDY1 <- matrix(rowMeans(VarCovIDY1),4,4)
  VarCovMeansIDY2 <- matrix(rowMeans(VarCovIDY2),4,4)
  VarCovMeansIDY3 <- matrix(rowMeans(VarCovIDY3),2,2)
  VarCovMeansIDY4 <- matrix(rowMeans(VarCovIDY4),4,4)
  VarCovMeansIDY5 <- matrix(rowMeans(VarCovIDY5),2,2)
  VarCovSDIDY1 <- matrix(apply(VarCovIDY1,1,sd),4,4)
  VarCovSDIDY2 <- matrix(apply(VarCovIDY2,1,sd),4,4)
  VarCovSDIDY3 <- matrix(apply(VarCovIDY3,1,sd),2,2)
  VarCovSDIDY4 <- matrix(apply(VarCovIDY4,1,sd),4,4)
  VarCovSDIDY5 <- matrix(apply(VarCovIDY5,1,sd),2,2)
  VarCov025IDY1 <- matrix(apply(VarCovIDY1,1,function(x) quantile(x, 0.025)),4,4)
  VarCov025IDY2 <- matrix(apply(VarCovIDY2,1,function(x) quantile(x, 0.025)),4,4)
  VarCov025IDY3 <- matrix(apply(VarCovIDY3,1,function(x) quantile(x, 0.025)),2,2)
  VarCov025IDY4 <- matrix(apply(VarCovIDY4,1,function(x) quantile(x, 0.025)),4,4)
  VarCov025IDY5 <- matrix(apply(VarCovIDY5,1,function(x) quantile(x, 0.025)),2,2)
  VarCov975IDY1 <- matrix(apply(VarCovIDY1,1,function(x) quantile(x, 0.975)),4,4)
  VarCov975IDY2 <- matrix(apply(VarCovIDY2,1,function(x) quantile(x, 0.975)),4,4)
  VarCov975IDY3 <- matrix(apply(VarCovIDY3,1,function(x) quantile(x, 0.975)),2,2)
  VarCov975IDY4 <- matrix(apply(VarCovIDY4,1,function(x) quantile(x, 0.975)),4,4)
  VarCov975IDY5 <- matrix(apply(VarCovIDY5,1,function(x) quantile(x, 0.975)),2,2)

  m.lstat.1 <- function(m) { #SD
    moments <- inla.emarginal(function(lx) c(exp(-lx/2), exp(-lx)), m)
    #q = inla.qmarginal(c(0.025,0.975), exp(-m/2))
    q = exp(-inla.qmarginal(c(0.025,0.975), m)/2)
    return(list(mean = moments[1], sd = sqrt(max(0, moments[2]-moments[1]^2)), "0.025quant"=q[2], "0.975quant"=q[1]))
  }
  resErr1_stat <- m.lstat.1(JMinla_Multi$internal.marginals.hyperpar$`Log precision for the lognormal observations`)
  resErr2_stat <- m.lstat.1(JMinla_Multi$internal.marginals.hyperpar$`Log precision for the lognormal observations[4]`)
  resErr3_stat <- m.lstat.1(JMinla_Multi$internal.marginals.hyperpar$`Log precision for the Gaussian observations[6]`)


  TV=c("$\\beta_{10}$ & ",
       "$\\beta_{11}$ & ",
       "$\\beta_{12}$ & ",
       "$\\beta_{13}$ & ",
       "$\\beta_{14}$ & ",
       "$\\beta_{15}$ & ",
       "$\\beta_{16}$ & ",
       "$\\beta_{17}$ & ",
       "$\\sigma_{\\varepsilon 1}$ & ",

       "$\\sigma_{b10}^2$ & ",
       "$\\sigma_{b11}^2$ & ",
       "$\\sigma_{b12}^2$ & ",
       "$\\sigma_{b13}^2$ & ",
       "$\\textrm{cov}_{b10,b11}$ & ",
       "$\\textrm{cov}_{b10,b12}$ & ",
       "$\\textrm{cov}_{b10,b13}$ & ",
       "$\\textrm{cov}_{b11,b12}$ & ",
       "$\\textrm{cov}_{b11,b13}$ & ",
       "$\\textrm{cov}_{b12,b13}$ & ",

       "$\\beta_{20}$ & ",
       "$\\beta_{21}$ & ",
       "$\\beta_{22}$ & ",
       "$\\beta_{23}$ & ",
       "$\\beta_{24}$ & ",
       "$\\beta_{25}$ & ",
       "$\\beta_{26}$ & ",
       "$\\beta_{27}$ & ",
       "$\\sigma_{\\varepsilon 2}$ & ",

       "$\\sigma_{b20}^2$ & ",
       "$\\sigma_{b21}^2$ & ",
       "$\\sigma_{b22}^2$ & ",
       "$\\sigma_{b23}^2$ & ",
       "$\\textrm{cov}_{b20,b21}$ & ",
       "$\\textrm{cov}_{b20,b22}$ & ",
       "$\\textrm{cov}_{b20,b23}$ & ",
       "$\\textrm{cov}_{b21,b22}$ & ",
       "$\\textrm{cov}_{b21,b23}$ & ",
       "$\\textrm{cov}_{b22,b23}$ & ",

       "$\\beta_{30}$ & ",
       "$\\beta_{31}$ & ",
       "$\\beta_{32}$ & ",
       "$\\beta_{33}$ & ",
       "$\\sigma_{\\varepsilon 3}$ & ",

       "$\\sigma_{b30}^2$ & ",
       "$\\sigma_{b31}^2$ & ",
       "$\\textrm{cov}_{b30,b31}$ & ",

       "$\\beta_{40}$ & ",
       "$\\beta_{41}$ & ",
       "$\\beta_{42}$ & ",
       "$\\beta_{43}$ & ",
       "$\\beta_{44}$ & ",
       "$\\beta_{45}$ & ",
       "$\\beta_{46}$ & ",
       "$\\beta_{47}$ & ",

       "$\\sigma_{b40}^2$ & ",
       "$\\sigma_{b41}^2$ & ",
       "$\\sigma_{b42}^2$ & ",
       "$\\sigma_{b43}^2$ & ",
       "$\\textrm{cov}_{b40,b41}$ & ",
       "$\\textrm{cov}_{b40,b42}$ & ",
       "$\\textrm{cov}_{b40,b43}$ & ",
       "$\\textrm{cov}_{b41,b42}$ & ",
       "$\\textrm{cov}_{b41,b43}$ & ",
       "$\\textrm{cov}_{b42,b43}$ & ",

       "$\\beta_{50}$ & ",
       "$\\beta_{51}$ & ",
       "$\\beta_{52}$ & ",
       "$\\beta_{53}$ & ",

       "$\\sigma_{b50}^2$ & ",
       "$\\sigma_{b51}^2$ & ",
       "$\\textrm{cov}_{b50,b51}$ & ",

       "$\\gamma_{1}$ & ",
       "$\\gamma_{2}$ & ",

       "$\\varphi_1$ & ",
       "$\\varphi_2$ & ",
       "$\\varphi_3$ & ",
       "$\\varphi_4$ & ",
       "$\\varphi_5$ & ",
       "$\\varphi_6$ & ",
       "$\\varphi_7$ & ",
       "$\\varphi_8$ & ",
       "$\\varphi_9$ & "
  )

  # "$\\sigma_{\\varepsilon 1}$=0.4 & ",
  # "$\\sigma_{b01}^2$=0.16 & ",
  # "$\\sigma_{b11}^2$=0.16 & ",
  # "$\\textrm{cov}_{b01,b11}$=0.08 & ",
  # "$\\varphi_1$=0.5 & ")


  HP <- round(rbind(cbind("mean"=c(diag(VarCovMeansIDY1), VarCovMeansIDY1[lower.tri(VarCovMeansIDY1)], diag(VarCovMeansIDY2), VarCovMeansIDY2[lower.tri(VarCovMeansIDY2)],
                                   diag(VarCovMeansIDY3), VarCovMeansIDY3[lower.tri(VarCovMeansIDY3)], diag(VarCovMeansIDY4), VarCovMeansIDY4[lower.tri(VarCovMeansIDY4)],
                                   diag(VarCovMeansIDY5), VarCovMeansIDY5[lower.tri(VarCovMeansIDY5)]),
                          "sd"=c(diag(VarCovSDIDY1), VarCovSDIDY1[lower.tri(VarCovSDIDY1)], diag(VarCovSDIDY2), VarCovSDIDY2[lower.tri(VarCovSDIDY2)],
                                 diag(VarCovSDIDY3), VarCovSDIDY3[lower.tri(VarCovSDIDY3)], diag(VarCovSDIDY4), VarCovSDIDY4[lower.tri(VarCovSDIDY4)],
                                 diag(VarCovSDIDY5), VarCovSDIDY5[lower.tri(VarCovSDIDY5)]),
                          "0.025quant"=c(diag(VarCov025IDY1), VarCov025IDY1[lower.tri(VarCov025IDY1)], diag(VarCov025IDY2), VarCov025IDY2[lower.tri(VarCov025IDY2)],
                                         diag(VarCov025IDY3), VarCov025IDY3[lower.tri(VarCov025IDY3)], diag(VarCov025IDY4), VarCov025IDY4[lower.tri(VarCov025IDY4)],
                                         diag(VarCov025IDY5), VarCov025IDY5[lower.tri(VarCov025IDY5)]),
                          "0.975quant"=c(diag(VarCov975IDY1), VarCov975IDY1[lower.tri(VarCov975IDY1)], diag(VarCov975IDY2), VarCov975IDY2[lower.tri(VarCov975IDY2)],
                                         diag(VarCov975IDY3), VarCov975IDY3[lower.tri(VarCov975IDY3)], diag(VarCov975IDY4), VarCov975IDY4[lower.tri(VarCov975IDY4)],
                                         diag(VarCov975IDY5), VarCov975IDY5[lower.tri(VarCov975IDY5)]))), 2)


  FE <- round(rbind(JMinla_Multi$summary.fixed[5:12,c(1:3,5)], c(unlist(resErr1_stat)),HP[1:10,],
                    JMinla_Multi$summary.fixed[13:20,c(1:3,5)], c(unlist(resErr2_stat)),HP[11:20,],
                    JMinla_Multi$summary.fixed[21:24,c(1:3,5)], c(unlist(resErr3_stat)),HP[21:23,],
                    JMinla_Multi$summary.fixed[25:32,c(1:3,5)],HP[24:33,],
                    JMinla_Multi$summary.fixed[33:36,c(1:3,5)],HP[34:36,],
                    JMinla_Multi$summary.fixed[c(2,4),c(1:3,5)],
                    JMinla_Multi$summary.hyperpar[42:50,c(1:3,5)]), 2)

  RE <- FE
  dim(RE)
  length(TV)
  RE1 <- RE[1:28,]
  RE2 <- RE[29:55,]
  RE3 <- RE[56:82,]
  TV1 <- TV[1:28]
  TV2 <- TV[29:55]
  TV3 <- TV[56:82]

  INLA_stats_tex <- cbind(paste0(TV1, RE1[, "mean"], " & (", RE1[, "sd"], ") ", "& [", RE1[, "0.025quant"], " ; ", RE1[, "0.975quant"], "] &",
                                 TV2, RE2[, "mean"], " & (", RE2[, "sd"], ") ", "& [", RE2[, "0.025quant"], " ; ", RE2[, "0.975quant"], "] &",
                                 TV3, RE3[, "mean"], " & (", RE3[, "sd"], ") ", "& [", RE3[, "0.025quant"], " ; ", RE3[, "0.975quant"], "] \\\\\n"))

  # RE1 <- RE[1:31,]
  # RE2 <- RE[32:76,]
  # TV1 <- TV[1:31]
  # TV2 <- TV[32:76]
  #
  # INLA_stats_tex <- cbind(paste0(TV1, RE1[, "mean"], " & (", RE1[, "sd"], ") ", "& [", RE1[, "0.025quant"], " ; ", RE1[, "0.975quant"], "] &",
  #                                TV2, RE2[, "mean"], " & (", RE2[, "sd"], ") ", "& [", RE2[, "0.025quant"], " ; ", RE2[, "0.975quant"], "] \\\\\n"))
  #INLA_stats_tex <- cbind(paste0(TV, RE[, "mean"], " & (", RE[, "sd"], ") ", "& [", RE[, "0.025quant"], " ; ", RE[, "0.975quant"], "] \\\\\n"))


  cat(c(INLA_stats_tex))













  # plot baseline survival
  if(F){
    summary(JMinla_Multi)
    plot(JMinla_Multi$internal.marginals.hyperpar$`Log precision for baseline.hazard`, type='l')
    plot(JMinla_Multi$internal.marginals.hyperpar$`Log precision for baselineTR.hazard`, type='l')

    #Baseline risk plot
    h0 <- lapply(JMinla_Multi$marginals.random[["baseline.hazard"]], function (X) {inla.tmarginal(exp, X)})
    h0.stats <- lapply(h0, inla.zmarginal)
    h0.df <- data.frame(t=JMinla_Multi$summary.random$baseline.hazard$ID)
    h0.df <- cbind(h0.df, do.call(rbind, lapply(h0.stats, unlist)))
    HazM <- h0.df$quant0.5
    HazL <- h0.df$quant0.025
    HazU <- h0.df$quant0.975
    tpts <- h0.df$t
    weightsI <- rep(tpts[2]-tpts[1],len=length(tpts))
    hazardEstINLA <- rbind(HazM, HazL, HazU)
    # risk
    par(mfrow=c(1,2), mar = c(4, 4, 1, 1))
    plot(tpts,HazM,type='l', lwd=2, lty=0, ylim=c(0,2.5),#,xlim=c(0,4),
         xlab="Time (years)", ylab="Baseline risk of death")
    lines(tpts, HazM, lwd=3, lty=1)
    lines(tpts, HazL, lwd=2, lty=1)
    lines(tpts, HazU, lwd=2, lty=1)

    h0 <- lapply(JMinla_Multi$marginals.random[["baselineTR.hazard"]], function (X) {inla.tmarginal(exp, X)})
    h0.stats <- lapply(h0, inla.zmarginal)
    h0.df <- data.frame(t=JMinla_Multi$summary.random$baseline.hazard$ID)
    h0.df <- cbind(h0.df, do.call(rbind, lapply(h0.stats, unlist)))
    HazM <- h0.df$quant0.5
    HazL <- h0.df$quant0.025
    HazU <- h0.df$quant0.975
    tpts <- h0.df$t
    weightsI <- rep(tpts[2]-tpts[1],len=length(tpts))
    hazardEstINLA <- rbind(HazM, HazL, HazU)
    # risk
    plot(tpts,HazM,type='l', lwd=2, lty=0, ylim=c(0,2.5),#xlim=c(0,4),
         xlab="Time (years)", ylab="Baseline risk of transplantation")
    lines(tpts, HazM, lwd=3, lty=1)
    lines(tpts, HazL, lwd=2, lty=1)
    lines(tpts, HazU, lwd=2, lty=1)





    #Baseline survival plot (not meaningful for covariates=0 with this model)
    h0 <- lapply(JMinla_Multi$marginals.random[["baseline.hazard"]], function (X) {inla.tmarginal(exp, X)})
    h0.stats <- lapply(h0, inla.zmarginal)
    h0.df <- data.frame(t=JMinla_Multi$summary.random$baseline.hazard$ID)
    h0.df <- cbind(h0.df, do.call(rbind, lapply(h0.stats, unlist)))
    HazM <- h0.df$quant0.5
    HazL <- h0.df$quant0.025
    HazU <- h0.df$quant0.975
    tpts <- h0.df$t
    weightsI <- rep(tpts[2]-tpts[1],len=length(tpts))
    # baseline survival
    survIM=exp(-(cumsum(HazM)*weightsI)) # baseline cumulative risk
    survIL=exp(-(cumsum(HazL)*weightsI)) # baseline cumulative risk
    survIU=exp(-(cumsum(HazU)*weightsI)) # baseline cumulative risk
    survIM <- survIM <- c(1,survIM[-1])#??
    survIL <- survIL <- c(1,survIL[-1])
    survIU <- survIU <-  c(1,survIU[-1])
    hazardEstINLA <- rbind(HazM, HazL, HazU)
    # risk
    ## survival
    plot(tpts,HazM, main="Baseline survival (death)",type='l', lwd=2, lty=0, ylim=c(0,1),#,xlim=c(0,4),
         xlab="Time (years)", ylab="Survival")
    lines(tpts, survIM, col='blue', lwd=3, lty=1)
    lines(tpts, survIL, col='blue', lwd=2, lty=1)
    lines(tpts, survIU, col='blue', lwd=2, lty=1)
    h0 <- lapply(JMinla_Multi$marginals.random[["baselineTR.hazard"]], function (X) {inla.tmarginal(exp, X)})
    h0.stats <- lapply(h0, inla.zmarginal)
    h0.df <- data.frame(t=JMinla_Multi$summary.random$baseline.hazard$ID)
    h0.df <- cbind(h0.df, do.call(rbind, lapply(h0.stats, unlist)))
    HazM <- h0.df$quant0.5
    HazL <- h0.df$quant0.025
    HazU <- h0.df$quant0.975
    tpts <- h0.df$t
    weightsI <- rep(tpts[2]-tpts[1],len=length(tpts))
    # baseline survival
    survIM=exp(-(cumsum(HazM)*weightsI)) # baseline cumulative risk
    survIL=exp(-(cumsum(HazL)*weightsI)) # baseline cumulative risk
    survIU=exp(-(cumsum(HazU)*weightsI)) # baseline cumulative risk
    survIM <- survIM <- c(1,survIM[-1])
    survIL <- survIL <- c(1,survIL[-1])
    survIU <- survIU <-  c(1,survIU[-1])
    hazardEstINLA <- rbind(HazM, HazL, HazU)
    # risk
    ## survival
    plot(tpts,survIM, main="Baseline survival (transplantation)",type='l', lwd=2, lty=0, ylim=c(0,1), #,xlim=c(0,4),
         xlab="Time (years)", ylab="Survival")
    lines(tpts, survIM, col='blue', lwd=3, lty=1)
    lines(tpts, survIL, col='blue', lwd=2, lty=1)
    lines(tpts, survIU, col='blue', lwd=2, lty=1)



    # observed vs fitted
    PD <- JMinla_Multi$summary.linear.predictor
    PDY1 <- PD[1:NL,]
    LongiM <- Longi
    LongiM$PDY1 <- PDY1$mean
    LongiR <- LongiM[LongiM$id %in%c(1:10),]
    # plot continuous variables
    require(ggplot2)
    require(ggvis)
    require(gridExtra)  ## required to arrange ggplot2 plots in a grid
    library(grid)
    library(gtable)
    library(PtProcess)
    library(ggpubr)
    mytheme <- theme_classic() %+replace%
      theme(axis.title.x = element_blank(),
            axis.title.y = element_text(face="bold",angle=90))
    p1 <- ggplot(data = LongiR, aes(x = year, y = log(serBilir), group = id, colour = as.factor(id))) +
      mytheme + #ylim(-2.5,4)+
      geom_line(size=0.5, linetype="dotted")+ theme(legend.position="bottom")+
      geom_line(aes(x = year, y = PDY1, group = id, colour = as.factor(id)), size=0.5)+ theme(legend.position="none")
    p1














    # Population mean survival plot
    # death
    h0 <- lapply(JMinla_Multi$marginals.random[["baseline.hazard"]], function (X) {inla.tmarginal(exp, X)})
    h0.stats <- lapply(h0, inla.zmarginal)
    h0.df <- data.frame(t=JMinla_Multi$summary.random$baseline.hazard$ID)
    h0.df <- cbind(h0.df, do.call(rbind, lapply(h0.stats, unlist)))
    HazM <- h0.df$quant0.5
    HazL <- h0.df$quant0.025
    HazU <- h0.df$quant0.975
    tpts <- h0.df$t
    weightsI <- rep(tpts[2]-tpts[1],len=length(tpts))
    hazardEstINLA <- rbind(HazM, HazL, HazU)
    tpoints <- seq(0,14,by=0.1)
    tpointsINLA <- tpts
    HM <- NULL
    HU <- NULL
    HL <- NULL
    for(i in 1:length(tpoints)){
      for(j in 2:length(hazardEstINLA[1,])){
        if(tpoints[i]<tpointsINLA[j] & tpoints[i]>=tpointsINLA[j-1]){
          regL_M <- lm(hazardEstINLA[1,c(j-1, j)]~tpointsINLA[c(j-1, j)])
          hazRegL_M <- regL_M$coefficients[1]+tpoints[i]*regL_M$coefficients[2]
          HM <- c(HM, hazRegL_M)
          regL_L <- lm(hazardEstINLA[2,c(j-1, j)]~tpointsINLA[c(j-1, j)])
          hazRegL_L <- regL_L$coefficients[1]+tpoints[i]*regL_L$coefficients[2]
          HL <- c(HL, hazRegL_L)
          regL_U <- lm(hazardEstINLA[3,c(j-1, j)]~tpointsINLA[c(j-1, j)])
          hazRegL_U <- regL_U$coefficients[1]+tpoints[i]*regL_U$coefficients[2]
          HU <- c(HU, hazRegL_U)
        }
      }
    }

    Nspl <- ns(tpoints, knots=c(1,4))
    DNspl <- dns(tpoints, knots=c(1,4))
    weights <- rep(tpoints[2]-tpoints[1],len=length(tpoints))

    # linear predictors
    Y1T <- JMinla_Multi$summary.fixed["InteY1","mean"] + Nspl[,1]*JMinla_Multi$summary.fixed["NS1Y1","mean"] +
      Nspl[,2]*JMinla_Multi$summary.fixed["NS2Y1","mean"] + Nspl[,3]*JMinla_Multi$summary.fixed["NS3Y1","mean"]
    Y1TD <- JMinla_Multi$summary.fixed["InteY1","mean"] +
      Nspl[,1]*(JMinla_Multi$summary.fixed["NS1Y1","mean"] + JMinla_Multi$summary.fixed["NS1drugY1","mean"]) +
      Nspl[,2]*(JMinla_Multi$summary.fixed["NS2Y1","mean"] + JMinla_Multi$summary.fixed["NS2drugY1","mean"]) +
      Nspl[,3]*(JMinla_Multi$summary.fixed["NS3Y1","mean"] + JMinla_Multi$summary.fixed["NS3drugY1","mean"]) +
      JMinla_Multi$summary.fixed["drugY1","mean"]

    Y1ST <- DNspl[,1]*JMinla_Multi$summary.fixed["NS1Y1","mean"] +
      DNspl[,2]*JMinla_Multi$summary.fixed["NS2Y1","mean"] + DNspl[,3]*JMinla_Multi$summary.fixed["NS3Y1","mean"]
    Y1STD <- DNspl[,1]*(JMinla_Multi$summary.fixed["NS1Y1","mean"] + JMinla_Multi$summary.fixed["NS1drugY1","mean"]) +
      DNspl[,2]*(JMinla_Multi$summary.fixed["NS2Y1","mean"] + JMinla_Multi$summary.fixed["NS2drugY1","mean"]) +
      DNspl[,3]*(JMinla_Multi$summary.fixed["NS3Y1","mean"] + JMinla_Multi$summary.fixed["NS3drugY1","mean"])

    Y2T <- JMinla_Multi$summary.fixed["InteY2","mean"] + Nspl[,1]*JMinla_Multi$summary.fixed["NS1Y2","mean"] +
      Nspl[,2]*JMinla_Multi$summary.fixed["NS2Y2","mean"] + Nspl[,3]*JMinla_Multi$summary.fixed["NS3Y2","mean"]
    Y2TD <- JMinla_Multi$summary.fixed["InteY2","mean"] +
      Nspl[,1]*(JMinla_Multi$summary.fixed["NS1Y2","mean"] + JMinla_Multi$summary.fixed["NS1drugY2","mean"]) +
      Nspl[,2]*(JMinla_Multi$summary.fixed["NS2Y2","mean"] + JMinla_Multi$summary.fixed["NS2drugY2","mean"]) +
      Nspl[,3]*(JMinla_Multi$summary.fixed["NS3Y2","mean"] + JMinla_Multi$summary.fixed["NS3drugY2","mean"]) +
      JMinla_Multi$summary.fixed["drugY2","mean"]

    Y3T <- JMinla_Multi$summary.fixed["InteY3","mean"] + tpoints*JMinla_Multi$summary.fixed["TIMEY3","mean"]
    Y3TD <- JMinla_Multi$summary.fixed["InteY3","mean"] +
      tpoints*(JMinla_Multi$summary.fixed["TIMEY3","mean"] + JMinla_Multi$summary.fixed["TIMEdrugY3","mean"]) +
      JMinla_Multi$summary.fixed["drugY3","mean"]

    Y4T <- JMinla_Multi$summary.fixed["InteY4","mean"] + Nspl[,1]*JMinla_Multi$summary.fixed["NS1Y4","mean"] +
      Nspl[,2]*JMinla_Multi$summary.fixed["NS2Y4","mean"] + Nspl[,3]*JMinla_Multi$summary.fixed["NS3Y4","mean"]
    Y4TD <- JMinla_Multi$summary.fixed["InteY4","mean"] +
      Nspl[,1]*(JMinla_Multi$summary.fixed["NS1Y4","mean"] + JMinla_Multi$summary.fixed["NS1drugY4","mean"]) +
      Nspl[,2]*(JMinla_Multi$summary.fixed["NS2Y4","mean"] + JMinla_Multi$summary.fixed["NS2drugY4","mean"]) +
      Nspl[,3]*(JMinla_Multi$summary.fixed["NS3Y4","mean"] + JMinla_Multi$summary.fixed["NS3drugY4","mean"]) +
      JMinla_Multi$summary.fixed["drugY4","mean"]

    Y5T <- JMinla_Multi$summary.fixed["InteY5","mean"] + tpoints*JMinla_Multi$summary.fixed["TIMEY5","mean"]
    Y5TD <- JMinla_Multi$summary.fixed["InteY5","mean"] +
      tpoints*(JMinla_Multi$summary.fixed["TIMEY5","mean"] + JMinla_Multi$summary.fixed["TIMEdrugY5","mean"]) +
      JMinla_Multi$summary.fixed["drugY5","mean"]

    LP <- Y1T*JMinla_Multi$summary.hyperpar["Beta for CV_Y1", "mean"] +
      Y1ST*JMinla_Multi$summary.hyperpar["Beta for CS_Y1", "mean"] +
      Y2T*JMinla_Multi$summary.hyperpar["Beta for CV_Y2", "mean"] +
      Y3T*JMinla_Multi$summary.hyperpar["Beta for CV_Y3", "mean"] +
      Y4T*JMinla_Multi$summary.hyperpar["Beta for CV_Y4", "mean"] +
      Y5T*JMinla_Multi$summary.hyperpar["Beta for CV_Y5", "mean"]
    LPD <- JMinla_Multi$summary.fixed["drug","mean"] + Y1TD*JMinla_Multi$summary.hyperpar["Beta for CV_Y1", "mean"] +
      Y1STD*JMinla_Multi$summary.hyperpar["Beta for CS_Y1", "mean"] +
      Y2TD*JMinla_Multi$summary.hyperpar["Beta for CV_Y2", "mean"] +
      Y3TD*JMinla_Multi$summary.hyperpar["Beta for CV_Y3", "mean"] +
      Y4TD*JMinla_Multi$summary.hyperpar["Beta for CV_Y4", "mean"] +
      Y5TD*JMinla_Multi$summary.hyperpar["Beta for CV_Y5", "mean"]

    # baseline surv
    SV <- exp(-cumsum(HM)*weights)
    survIM <- SV^exp(LP)
    survIMD <- SV^exp(LPD)

    plot(tpoints,survIM, main="Survival",type='l', lwd=2, lty=0, ylim=c(0,1),#xlim=c(0,4),
         xlab="Time (years)", ylab="Survival")
    lines(tpoints, survIM, col='blue', lwd=2, lty=1)
    lines(tpoints, survIMD, col='red', lwd=2, lty=2)


    # confidence intervals (Monte-carlo method)
    nloop=1000 # number of Monte-Carlo curves
    survMC=NULL
    survMCtrt=NULL
    for(i in 1:nloop){
      SMP <- inla.posterior.sample(1,JMinla_Multi)
      Hyper <- inla.hyperpar.sample(1,JMinla_Multi)
      FixedEff <- tail(SMP[[1]]$latent[,1], n=36)

      Y1T <- FixedEff["InteY1:1"] + Nspl[,1]*FixedEff["NS1Y1:1"] +
        Nspl[,2]*FixedEff["NS2Y1:1"] + Nspl[,3]*FixedEff["NS3Y1:1"]
      Y1TD <- FixedEff["InteY1:1"] +
        Nspl[,1]*(FixedEff["NS1Y1:1"] + FixedEff["NS1drugY1:1"]) +
        Nspl[,2]*(FixedEff["NS2Y1:1"] + FixedEff["NS2drugY1:1"]) +
        Nspl[,3]*(FixedEff["NS3Y1:1"] + FixedEff["NS3drugY1:1"]) +
        FixedEff["drugY1:1"]

      Y1ST <- DNspl[,1]*FixedEff["NS1Y1:1"] +
        DNspl[,2]*FixedEff["NS2Y1:1"] + DNspl[,3]*FixedEff["NS3Y1:1"]
      Y1STD <- DNspl[,1]*(FixedEff["NS1Y1:1"] + FixedEff["NS1drugY1:1"]) +
        DNspl[,2]*(FixedEff["NS2Y1:1"] + FixedEff["NS2drugY1:1"]) +
        DNspl[,3]*(FixedEff["NS3Y1:1"] + FixedEff["NS3drugY1:1"])

      Y2T <- FixedEff["InteY2:1"] + Nspl[,1]*FixedEff["NS1Y2:1"] +
        Nspl[,2]*FixedEff["NS2Y2:1"] + Nspl[,3]*FixedEff["NS3Y2:1"]
      Y2TD <- FixedEff["InteY2:1"] +
        Nspl[,1]*(FixedEff["NS1Y2:1"] + FixedEff["NS1drugY2:1"]) +
        Nspl[,2]*(FixedEff["NS2Y2:1"] + FixedEff["NS2drugY2:1"]) +
        Nspl[,3]*(FixedEff["NS3Y2:1"] + FixedEff["NS3drugY2:1"]) +
        FixedEff["drugY2:1"]

      Y3T <- FixedEff["InteY3:1"] + tpoints*FixedEff["TIMEY3:1"]
      Y3TD <- FixedEff["InteY3:1"] +
        tpoints*(FixedEff["TIMEY3:1"] + FixedEff["TIMEdrugY3:1"]) +
        FixedEff["drugY3:1"]

      Y4T <- FixedEff["InteY4:1"] + Nspl[,1]*FixedEff["NS1Y4:1"] +
        Nspl[,2]*FixedEff["NS2Y4:1"] + Nspl[,3]*FixedEff["NS3Y4:1"]
      Y4TD <- FixedEff["InteY4:1"] +
        Nspl[,1]*(FixedEff["NS1Y4:1"] + FixedEff["NS1drugY4:1"]) +
        Nspl[,2]*(FixedEff["NS2Y4:1"] + FixedEff["NS2drugY4:1"]) +
        Nspl[,3]*(FixedEff["NS3Y4:1"] + FixedEff["NS3drugY4:1"]) +
        FixedEff["drugY4:1"]

      Y5T <- FixedEff["InteY5:1"] + tpoints*FixedEff["TIMEY5:1"]
      Y5TD <- FixedEff["InteY5:1"] +
        tpoints*(FixedEff["TIMEY5:1"] + FixedEff["TIMEdrugY5:1"]) +
        FixedEff["drugY5:1"]

      LP <- Y1T*Hyper[1,"Beta for CV_Y1"] +
        Y1ST*Hyper[1,"Beta for CS_Y1"] +
        Y2T*Hyper[1,"Beta for CV_Y2"] +
        Y3T*Hyper[1,"Beta for CV_Y3"] +
        Y4T*Hyper[1,"Beta for CV_Y4"] +
        Y5T*Hyper[1,"Beta for CV_Y5"]
      LPD <- FixedEff["drug:1"] + Y1TD*Hyper[1,"Beta for CV_Y1"] +
        Y1STD*Hyper[1,"Beta for CS_Y1"] +
        Y2TD*Hyper[1,"Beta for CV_Y2"] +
        Y3TD*Hyper[1,"Beta for CV_Y3"] +
        Y4TD*Hyper[1,"Beta for CV_Y4"] +
        Y5TD*Hyper[1,"Beta for CV_Y5"]

      # baseline surv
      SV <- exp(-cumsum(HM)*weights)
      survMC <- cbind(survMC,  SV^exp(LP))
      survMCtrt <- cbind(survMCtrt, SV^exp(LPD))
    }

    # quantiles
    QL <- function(x) quantile(x,prob=0.025)
    QM <- function(x) quantile(x,prob=0.5)
    QU <- function(x) quantile(x,prob=0.975)
    SCL <- apply(survMC,1,QL) # ref lower
    SCM<- apply(survMC,1,QM) # ref Median
    SCU <- apply(survMC,1,QU) # ref upper
    SCLtrt <- apply(survMCtrt,1,QL) # trt lower
    SCMtrt <- apply(survMCtrt,1,QM) # trt lower
    SCUtrt <- apply(survMCtrt,1,QU) # trt upper

    # plot
    par(mfrow=c(1,2))
    plot(tpoints,SCM,lwd=2,xlab="Time (years)",ylab="Population mean survival (death)",ylim=c(0,1),type='l')
    lines(tpoints,SCMtrt,col='red',lwd=2,lty=2)
    lines(tpoints,SCL)
    lines(tpoints,SCU)
    lines(tpoints,SCLtrt,col='red',lty=2)
    lines(tpoints,SCUtrt,col='red',lty=2)
    legend("bottomleft",title = "Treatment", c("placebo","D-penicil"), lty=c(1,2),
           lwd=c(2,2),col=c("black","red"),bty = "n")

    # 65% deces predits mais 45% observes => a cause de la censure?
















    # transplantation
    h0 <- lapply(JMinla_Multi$marginals.random[["baselineTR.hazard"]], function (X) {inla.tmarginal(exp, X)})
    h0.stats <- lapply(h0, inla.zmarginal)
    h0.df <- data.frame(t=JMinla_Multi$summary.random$baselineTR.hazard$ID)
    h0.df <- cbind(h0.df, do.call(rbind, lapply(h0.stats, unlist)))
    HazM <- h0.df$quant0.5
    HazL <- h0.df$quant0.025
    HazU <- h0.df$quant0.975
    tpts <- h0.df$t
    weightsI <- rep(tpts[2]-tpts[1],len=length(tpts))
    hazardEstINLA <- rbind(HazM, HazL, HazU)
    tpoints <- seq(0,14,by=0.1)
    tpointsINLA <- tpts
    HM <- NULL
    HU <- NULL
    HL <- NULL
    for(i in 1:length(tpoints)){
      for(j in 2:length(hazardEstINLA[1,])){
        if(tpoints[i]<tpointsINLA[j] & tpoints[i]>=tpointsINLA[j-1]){
          regL_M <- lm(hazardEstINLA[1,c(j-1, j)]~tpointsINLA[c(j-1, j)])
          hazRegL_M <- regL_M$coefficients[1]+tpoints[i]*regL_M$coefficients[2]
          HM <- c(HM, hazRegL_M)
          regL_L <- lm(hazardEstINLA[2,c(j-1, j)]~tpointsINLA[c(j-1, j)])
          hazRegL_L <- regL_L$coefficients[1]+tpoints[i]*regL_L$coefficients[2]
          HL <- c(HL, hazRegL_L)
          regL_U <- lm(hazardEstINLA[3,c(j-1, j)]~tpointsINLA[c(j-1, j)])
          hazRegL_U <- regL_U$coefficients[1]+tpoints[i]*regL_U$coefficients[2]
          HU <- c(HU, hazRegL_U)
        }
      }
    }

    Nspl <- ns(tpoints, knots=c(1,4))
    DNspl <- dns(tpoints, knots=c(1,4))
    weights <- rep(tpoints[2]-tpoints[1],len=length(tpoints))

    # linear predictors
    Y1T <- JMinla_Multi$summary.fixed["InteY1","mean"] + Nspl[,1]*JMinla_Multi$summary.fixed["NS1Y1","mean"] +
      Nspl[,2]*JMinla_Multi$summary.fixed["NS2Y1","mean"] + Nspl[,3]*JMinla_Multi$summary.fixed["NS3Y1","mean"]
    Y1TD <- JMinla_Multi$summary.fixed["InteY1","mean"] +
      Nspl[,1]*(JMinla_Multi$summary.fixed["NS1Y1","mean"] + JMinla_Multi$summary.fixed["NS1drugY1","mean"]) +
      Nspl[,2]*(JMinla_Multi$summary.fixed["NS2Y1","mean"] + JMinla_Multi$summary.fixed["NS2drugY1","mean"]) +
      Nspl[,3]*(JMinla_Multi$summary.fixed["NS3Y1","mean"] + JMinla_Multi$summary.fixed["NS3drugY1","mean"]) +
      JMinla_Multi$summary.fixed["drugY1","mean"]

    Y3T <- JMinla_Multi$summary.fixed["InteY3","mean"] + tpoints*JMinla_Multi$summary.fixed["TIMEY3","mean"]
    Y3TD <- JMinla_Multi$summary.fixed["InteY3","mean"] +
      tpoints*(JMinla_Multi$summary.fixed["TIMEY3","mean"] + JMinla_Multi$summary.fixed["TIMEdrugY3","mean"]) +
      JMinla_Multi$summary.fixed["drugY3","mean"]

    Y4T <- JMinla_Multi$summary.fixed["InteY4","mean"] + Nspl[,1]*JMinla_Multi$summary.fixed["NS1Y4","mean"] +
      Nspl[,2]*JMinla_Multi$summary.fixed["NS2Y4","mean"] + Nspl[,3]*JMinla_Multi$summary.fixed["NS3Y4","mean"]
    Y4TD <- JMinla_Multi$summary.fixed["InteY4","mean"] +
      Nspl[,1]*(JMinla_Multi$summary.fixed["NS1Y4","mean"] + JMinla_Multi$summary.fixed["NS1drugY4","mean"]) +
      Nspl[,2]*(JMinla_Multi$summary.fixed["NS2Y4","mean"] + JMinla_Multi$summary.fixed["NS2drugY4","mean"]) +
      Nspl[,3]*(JMinla_Multi$summary.fixed["NS3Y4","mean"] + JMinla_Multi$summary.fixed["NS3drugY4","mean"]) +
      JMinla_Multi$summary.fixed["drugY4","mean"]


    LP <- Y1T*JMinla_Multi$summary.hyperpar["Beta for CVTR_Y1", "mean"] +
      Y3T*JMinla_Multi$summary.hyperpar["Beta for CVTR_Y3", "mean"] +
      Y4T*JMinla_Multi$summary.hyperpar["Beta for CVTR_Y4", "mean"]
    LPD <- JMinla_Multi$summary.fixed["drugTR","mean"] + Y1TD*JMinla_Multi$summary.hyperpar["Beta for CVTR_Y1", "mean"] +
      Y3TD*JMinla_Multi$summary.hyperpar["Beta for CVTR_Y3", "mean"] +
      Y4TD*JMinla_Multi$summary.hyperpar["Beta for CVTR_Y4", "mean"]

    # baseline surv
    SV <- exp(-cumsum(HM)*weights)
    survIM <- SV^exp(LP)
    survIMD <- SV^exp(LPD)

    # plot(tpoints,survIM, main="Survival (transplantation)",type='l', lwd=2, lty=0, ylim=c(0,1),#xlim=c(0,4),
    #      xlab="Time (years)", ylab="Survival (transplantation)")
    # lines(tpoints, survIM, col='blue', lwd=2, lty=1)
    # lines(tpoints, survIMD, col='red', lwd=2, lty=2)
    #

    # confidence intervals (Monte-carlo method)
    nloop=1000 # number of Monte-Carlo curves
    survMC=NULL
    survMCtrt=NULL
    for(i in 1:nloop){
      SMP <- inla.posterior.sample(1,JMinla_Multi)
      Hyper <- inla.hyperpar.sample(1,JMinla_Multi)
      FixedEff <- tail(SMP[[1]]$latent[,1], n=36)

      Y1T <- FixedEff["InteY1:1"] + Nspl[,1]*FixedEff["NS1Y1:1"] +
        Nspl[,2]*FixedEff["NS2Y1:1"] + Nspl[,3]*FixedEff["NS3Y1:1"]
      Y1TD <- FixedEff["InteY1:1"] +
        Nspl[,1]*(FixedEff["NS1Y1:1"] + FixedEff["NS1drugY1:1"]) +
        Nspl[,2]*(FixedEff["NS2Y1:1"] + FixedEff["NS2drugY1:1"]) +
        Nspl[,3]*(FixedEff["NS3Y1:1"] + FixedEff["NS3drugY1:1"]) +
        FixedEff["drugY1:1"]

      Y1ST <- DNspl[,1]*FixedEff["NS1Y1:1"] +
        DNspl[,2]*FixedEff["NS2Y1:1"] + DNspl[,3]*FixedEff["NS3Y1:1"]
      Y1STD <- DNspl[,1]*(FixedEff["NS1Y1:1"] + FixedEff["NS1drugY1:1"]) +
        DNspl[,2]*(FixedEff["NS2Y1:1"] + FixedEff["NS2drugY1:1"]) +
        DNspl[,3]*(FixedEff["NS3Y1:1"] + FixedEff["NS3drugY1:1"])

      Y2T <- FixedEff["InteY2:1"] + Nspl[,1]*FixedEff["NS1Y2:1"] +
        Nspl[,2]*FixedEff["NS2Y2:1"] + Nspl[,3]*FixedEff["NS3Y2:1"]
      Y2TD <- FixedEff["InteY2:1"] +
        Nspl[,1]*(FixedEff["NS1Y2:1"] + FixedEff["NS1drugY2:1"]) +
        Nspl[,2]*(FixedEff["NS2Y2:1"] + FixedEff["NS2drugY2:1"]) +
        Nspl[,3]*(FixedEff["NS3Y2:1"] + FixedEff["NS3drugY2:1"]) +
        FixedEff["drugY2:1"]

      Y3T <- FixedEff["InteY3:1"] + tpoints*FixedEff["TIMEY3:1"]
      Y3TD <- FixedEff["InteY3:1"] +
        tpoints*(FixedEff["TIMEY3:1"] + FixedEff["TIMEdrugY3:1"]) +
        FixedEff["drugY3:1"]

      Y4T <- FixedEff["InteY4:1"] + Nspl[,1]*FixedEff["NS1Y4:1"] +
        Nspl[,2]*FixedEff["NS2Y4:1"] + Nspl[,3]*FixedEff["NS3Y4:1"]
      Y4TD <- FixedEff["InteY4:1"] +
        Nspl[,1]*(FixedEff["NS1Y4:1"] + FixedEff["NS1drugY4:1"]) +
        Nspl[,2]*(FixedEff["NS2Y4:1"] + FixedEff["NS2drugY4:1"]) +
        Nspl[,3]*(FixedEff["NS3Y4:1"] + FixedEff["NS3drugY4:1"]) +
        FixedEff["drugY4:1"]

      Y5T <- FixedEff["InteY5:1"] + tpoints*FixedEff["TIMEY5:1"]
      Y5TD <- FixedEff["InteY5:1"] +
        tpoints*(FixedEff["TIMEY5:1"] + FixedEff["TIMEdrugY5:1"]) +
        FixedEff["drugY5:1"]

      LP <- Y1T*Hyper[1,"Beta for CVTR_Y1"] +
        Y3T*Hyper[1,"Beta for CVTR_Y3"] +
        Y4T*Hyper[1,"Beta for CVTR_Y4"]
      LPD <- FixedEff["drugTR:1"] + Y1TD*Hyper[1,"Beta for CVTR_Y1"] +
        Y3TD*Hyper[1,"Beta for CVTR_Y3"] +
        Y4TD*Hyper[1,"Beta for CVTR_Y4"]

      # baseline surv
      SV <- exp(-cumsum(HM)*weights)
      survMC <- cbind(survMC,  SV^exp(LP))
      survMCtrt <- cbind(survMCtrt, SV^exp(LPD))
    }

    # quantiles
    QL <- function(x) quantile(x,prob=0.025)
    QM <- function(x) quantile(x,prob=0.5)
    QU <- function(x) quantile(x,prob=0.975)
    SCL_T <- apply(survMC,1,QL) # ref lower
    SCM_T <- apply(survMC,1,QM) # ref Median
    SCU_T <- apply(survMC,1,QU) # ref upper
    SCLtrt_T <- apply(survMCtrt,1,QL) # trt lower
    SCMtrt_T <- apply(survMCtrt,1,QM) # trt lower
    SCUtrt_T <- apply(survMCtrt,1,QU) # trt upper

    # plot
    plot(tpoints,SCM_T,lwd=2,xlab="Time (years)",ylab="Population mean survival (transplantation)",ylim=c(0,1),type='l')
    lines(tpoints,SCMtrt_T,col='red',lwd=2,lty=2)
    lines(tpoints,SCL_T)
    lines(tpoints,SCU_T)
    lines(tpoints,SCLtrt_T,col='red',lty=2)
    lines(tpoints,SCUtrt_T,col='red',lty=2)
    # legend("bottomleft",title = "Treatment", c("placebo","D-penicil"), lty=c(1,2),
    #        lwd=c(2,2),col=c("black","red"))





























    HMT <- HM*exp(Y1T*JMinla_Multi$summary.hyperpar["Beta for CV_Y1", "mean"] +
                    Y1ST*JMinla_Multi$summary.hyperpar["Beta for CS_Y1", "mean"] +
                    Y2T*JMinla_Multi$summary.hyperpar["Beta for CV_Y2", "mean"] +
                    Y3T*JMinla_Multi$summary.hyperpar["Beta for CV_Y3", "mean"] +
                    Y4T*JMinla_Multi$summary.hyperpar["Beta for CV_Y4", "mean"] +
                    Y5T*JMinla_Multi$summary.hyperpar["Beta for CV_Y5", "mean"])
    HMTD <- HM*exp(JMinla_Multi$summary.fixed["drug","mean"] + Y1TD*JMinla_Multi$summary.hyperpar["Beta for CV_Y1", "mean"] +
                     Y1STD*JMinla_Multi$summary.hyperpar["Beta for CS_Y1", "mean"] +
                     Y2TD*JMinla_Multi$summary.hyperpar["Beta for CV_Y2", "mean"] +
                     Y3TD*JMinla_Multi$summary.hyperpar["Beta for CV_Y3", "mean"] +
                     Y4TD*JMinla_Multi$summary.hyperpar["Beta for CV_Y4", "mean"] +
                     Y5TD*JMinla_Multi$summary.hyperpar["Beta for CV_Y5", "mean"])


    # HM <- c(0, HM)
    # HL <- c(0, HL)
    # HU <- c(0, HU)
    survIM=exp(-(cumsum(HMT)*weights)) # baseline cumulative risk
    survIMD=exp(-(cumsum(HMTD)*weights)) # baseline cumulative risk

    plot(tpoints,survIM, main="Survival",type='l', lwd=2, lty=0, #ylim=c(0,1),#xlim=c(0,4),
         xlab="Time (years)", ylab="Survival")
    lines(tpoints, survIM, col='blue', lwd=2, lty=1)
    lines(tpoints, survIMD, col='blue', lwd=2, lty=2)


    #nultiply by varphi!! for survival


    rm <- NULL
    for(i in 2:nrow(Longi)){
      if(Longi$id[i]==Longi$id[i-1]) rm <- c(rm, i)
    }

    Surv <- Longi[-rm,]
    dim(Surv)
    summary(Surv)








  }

































