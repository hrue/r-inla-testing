library(INLA)
inla.setOption(num.threads = "1:1")
inla.setOption(inla.call = "inla.mkl.work")
INLA:::inla.my.update()

group.size = 5
group.each = 3
n = group.size*group.each
group.mu = rnorm(group.size)
eta = 1 + rep(group.mu,each = group.each)
if (!exists("y"))
    y = rpois(n,exp(eta))
id = rep(1:group.size,each= group.each)
plot(y)

family = "poisson"
vb = FALSE

formula = y~ 1 + f(id,model = "iid",hyper = list(prec = list(prior =  "pc.prec",param = c(0.5, 0.01))))
res = inla(formula, family=family, data=list(id=id,y=y),
           inla.mode = "experimental",
           control.compute = list(control.gcpo = list(enable = TRUE,group.size = 2,verbose = T),config = T),
           control.inla = list(int.strategy = "grid",control.vb = list(enable = vb)),verbose = T)

res.off = inla(formula, family=family, data=list(id=id,y=y),
           inla.mode = "experimental",
           control.compute = list(control.gcpo = list(enable = TRUE,group.size = 2,verbose = T, correct.hyperpar = FALSE),config = T),
           control.inla = list(int.strategy = "grid",control.vb = list(enable = vb)),verbose = T)

##################gcpo computation refit
mg = 13
rule = fastGHQuad::gaussHermiteData(mg)
ww = rule$w
xx = rule$x
A = res$misc$configs$A
nconfig = res$misc$configs$nconfig
weights  = numeric(nconfig)
Design = matrix(NA,nconfig,res$misc$configs$ntheta+1)
for(config.interest in 1:nconfig){
  weights[config.interest] = exp(res$misc$configs$config[[config.interest]]$log.posterior - res$misc$configs$config[[config.interest]]$log.posterior.orig)
  Design[config.interest,] = c(res$misc$configs$config[[config.interest]]$theta,weights[config.interest])
}
Design[,2] = Design[,2]/sum(Design[,2])


group.fitted = NA
res.refit = numeric(n)
for(point.interest in 1:n) {
    if(!all(res$gcpo$groups[[point.interest]]$idx%in%group.fitted)){
        print(point.interest)
        group.fitted = res$gcpo$groups[[point.interest]]$idx
        y.leave = y
        y.leave[res$gcpo$groups[[point.interest]]$idx] = NA
        res.leave = inla(formula, family=family, data=list(id=id,y=y.leave),
                         inla.mode = "experimental",
                         control.compute = list(config = T),
                         control.predictor=list(link=1),
                         control.inla = list(int.strategy = "user",int.design = Design,control.vb = list(enable = vb))
                         )
    }
    res.refit.config = numeric(nconfig)
    weights.here = numeric(nconfig)
    for(config.interest in 1:nconfig){
        mu.nI = (A%*%res.leave$misc$configs$config[[config.interest]]$improved.mean)[point.interest]
        Qinv.beta = res.leave$misc$configs$config[[config.interest]]$Qinv + t(res.leave$misc$configs$config[[config.interest]]$Qinv)
        diag(Qinv.beta) = 0.5* diag(Qinv.beta)
        sd.nI = sqrt(sum(A[point.interest,]*(Qinv.beta%*%A[point.interest,])))
        x.here = sqrt(2)*sd.nI*xx + mu.nI
        res.refit.config[config.interest] = 1/sqrt(pi)*sum(ww*dpois(x=y[point.interest],lambda = exp(x.here)))
        weights.here[config.interest] = exp(res.leave$misc$configs$config[[config.interest]]$log.posterior)
    }
    weights.here = weights.here/sum(weights.here)
    res.refit[point.interest] = sum(res.refit.config*weights.here)
}


plot(res$gcpo$gcpo,res.refit, pch = 19, col = "red")
points(res.off$gcpo$gcpo,res.refit, col = "blue")
abline(a = 0, b = 1)
print(list( with.correction = mean(abs(inla.link.logit(res$gcpo$gcpo) - inla.link.logit(res.refit))), 
           without.correction = mean(abs(inla.link.logit(res.off$gcpo$gcpo) -
                                         inla.link.logit(res.refit)))))
