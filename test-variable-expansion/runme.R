nn = 3
mm = 3
yy = 1:mm
xx = 1:nn
iidx = 1:nn
pparam = c(1, 1)
ww = runif(nn)
sscal = runif(mm)
AA = matrix(nn*mm, mm, nn)
BB = matrix(nn*mm, nn, mm)

if (TRUE) {
    r = inla(y ~ 1, data = list(x=xx, y=yy))
    r = inla(y ~ 1 + x, data = list(x=xx, y=yy))
    r = inla(y ~ 1 + x + f(idx), data = list(x=xx, y=yy, idx=iidx))
    r = inla(y ~ 1 + x + f(idx, param = param), 
            data = list(x=xx, y=yy, idx=iidx, param = pparam))
    r = inla(y ~ 1 + x + f(idx, w, param = param), verbose=TRUE, 
            data = list(x=xx, y=yy, idx=iidx, param = pparam,
                    w=ww, scal = sscal),
            scale = scal)
}
if (TRUE) {
    r = inla(y ~ 1 + x + B + f(idx, w, param = param, values = val), verbose=TRUE, 
            data = list(x=xx, y=yy, idx=iidx, param = pparam,
                    w=ww, scal = sscal, A = AA, B = BB, val = 1:100), 
            control.predictor = list(A=A), 
            scale = scal)
}
if (TRUE) {
    data=as.list(read.table("mixeddata.txt",header=T))
    data$group[1:10] = 2
    data$cpoints=c(0,6,12,18,25)
    data$param = c(1, 1)
    ##data$A = matrix(runif(length(data$time)*12), length(data$time), 12)
    formula=inla.surv(time,event)~ group 
    model1=inla(formula,family="coxph", data=data,
            control.predictor = list(param = param), 
            control.hazard = list(cutpoints = cpoints),
            verbose=TRUE)
}
if (TRUE) {
    data=as.list(read.table("mixeddata.txt",header=T))
    data$group[1:10] = 2
    formula=inla.surv(time,event,subject=subject)~ group 
    model2=inla(formula,family="coxph", data=data)
}

