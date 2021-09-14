library(INLA)

## when data is in sequence (subjectwise)
data=read.table("data1.txt",header=T)
cutpoints=c(0,6,12,18,25)
formula= inla.surv(time,event,subject=subject) ~ group
model=inla(formula,family="coxph", data=data)

##--------when data is mixed
data=read.table("mixeddata.txt",header=T)
cutpoints=c(0,6,12,18,25)
yy=with(data, inla.surv(time,event,subject=subject))
formula= yy ~ group 
data = c(as.list(data),  list(yy=yy))
model1=inla(formula,family="coxph", data=data)

##- 
data=read.table("data4.txt", header=T)
yy=with(data, inla.surv(time,event,subject=subject))
formula= yy ~ trt + age
data = c(as.list(data),  list(yy=yy))
model=inla(formula,family="coxph", data=data)


## FOR REAL DATA
data=read.table("new.data.txt", header=T)
cutpoints=seq(62,182,by=24)
yy=with(data, inla.surv(time,event, subject=subject))
formula= yy ~ group 
data = c(as.list(data),  list(yy=yy))
model=inla(formula,family="coxph", data=data)

##for diff subject and without subject
data=read.table("data1.txt",header=T)
data$subject = 1:dim(data)[1]
cutpoints=c(0,6,12,18,25)
yy=with(data, inla.surv(time,event,subject=subject))
formula= yy ~ group 
data = c(as.list(data),  list(yy=yy))
model=inla(formula,family="coxph", data=data)

if (FALSE) {
    ## checking for fixed covariate
    data=read.table("data1.txt",header=T)
    data$z = rnorm(dim(data)[1])
    cutpoints=c(0,6,12,18,25)
    yy=with(data, inla.surv(time,event,subject=subject))
    formula= yy ~ group + z
    data = c(as.list(data),  list(yy=yy))
    model=inla(formula,family="coxph", data=data)
}
