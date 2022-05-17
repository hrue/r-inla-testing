library(INLAjoint)
library(JM) # This package contains the dataset
options(width=120)


data(pbc2) # dataset
# extract some variable of interest without missing values
Longi <- na.omit(pbc2[, c("id", "years", "status","drug","age", 
                          "sex","year","serBilir","SGOT", "albumin", "edema",
                          "platelets", "alkaline","spiders", "ascites")])
Surv <- Longi[c(which(diff(as.numeric(Longi[,which(colnames(Longi)=="id")]))==1),
                length(Longi[,which(colnames(Longi)=="id")])),-c(7:10, 12:16)]
Surv$death <- ifelse(Surv$status=="dead",1,0) # competing event 1
Surv$trans <- ifelse(Surv$status=="transplanted",1,0) # competing event 2


DTH <- inla.surv(time = Surv$years, event = Surv$death) # survival outcome
f1 <- function(x) x^2
f2 <- function(x) x^3

library(JMbayes)
M4JMB_lme <- lme(albumin ~ (1 + year)*drug,
                  random = ~ 1 + year |id, data = Longi)
M4JMB_cox <- coxph(Surv(Surv$years, Surv$death) ~ sex + drug,
                   data = Surv, x = TRUE)
JMpr = list(priorMean.alphas=0, priorTau.alphas = matrix(0.16))

TSP <- inla.surv(time = Surv$years, event = Surv$trans) 
data(SurvMS) # load small simulated dataset for multi-state
E12 <- inla.surv(time = SurvMS[[1]]$Tstop, event = SurvMS[[1]]$status) # transition 1->2
E13 <- inla.surv(time = SurvMS[[2]]$Tstop, event = SurvMS[[2]]$status) # transition 1->3
E23 <- inla.surv(time = SurvMS[[3]]$Tstop, truncation=SurvMS[[3]]$Tstart, event =SurvMS[[3]]$status) # transition 2->3

data(LongMS) # load longitudinal data for joint longitudinal and multi-state
Nsplines <- ns(Longi$year, knots=c(1,4))
f1 <- function(x) predict(Nsplines, x)[,1]
f2 <- function(x) predict(Nsplines, x)[,2]
f3 <- function(x) predict(Nsplines, x)[,3]

M9 <- joint(formLong = list(serBilir ~ (1 + f1(year) + f2(year) + f3(year)) * drug + 
                                       (1 + f1(year) + f2(year) + f3(year) | id),
                            SGOT ~ (1 + f1(year) + f2(year) + f3(year)) * drug + 
                                   (1 + f1(year) + f2(year) + f3(year) | id),
                            albumin ~ (1 + year) * drug + (1 + year | id),
                            platelets ~ (1 + f1(year) + f2(year) + f3(year)) * drug + 
                                        (1 + f1(year) + f2(year) + f3(year) | id),
                            spiders ~ (1 + year) * drug + (1 + year | id)),
            formSurv = list(DTH ~ drug, TSP ~ drug),
            dataLong = Longi, id = "id", timeVar = "year", basRisk = c("rw2", "rw1"), 
            family = c("lognormal", "lognormal", "gaussian", "poisson", "binomial"),
            assoc = list(c("CV_CS", "CV"),  c("CV", ""), c("CV", "CV"), 
                         c("CV", "CV"), c("CV", "")),
            control=list(priorFixed=list(mean=0, prec=0.16, 
                         mean.intercept=0, prec.intercept=0.16), 
                         priorAssoc=list(mean=0, prec=0.16),
                         priorRandom=list(r=10, R=1), keep = T, verbose = T, 
                         int.strategy="eb"))
summary(M9)
