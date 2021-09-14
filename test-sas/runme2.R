source("~/p/assembla/reproject/R/sas.R")
inla.my.update(b=TRUE)
n = 300
y = rsas(n, skew=1, kurt=4)
idx = 1:n
formula = y ~ 1

r = inla(formula,  data = data.frame(y, idx),
        family = "sas", 
        control.family = list(hyper = list(
                                      prec = list(initial = 0,  fixed=TRUE),
                                      skew = list(initial = 0, param = 12),
                                      kurt = list(initial = 3))),
        control.inla = list(h=0.1), 
        verbose=TRUE, keep=TRUE)
rr = inla(formula,  data = data.frame(y, idx),
        family = "gaussian",
        control.family = list(hyper = list(prec = list(initial = 0, fixed=TRUE))))

print(c(r$mlik[1, 1],  rr$mlik[1, 1]))
print(paste("In favour of SAS",  c(r$mlik[1, 1] -rr$mlik[1, 1])))

