library(INLA)
library(fastGHQuad)
library(numDeriv)

inla.setOption(num.threads = "1:1")
inla.setOption(inla.call = NULL)
inla.setOption(inla.call = "inla.mkl.work")
##INLA:::inla.my.update()

group.size = 500
group.each = 1
n = group.size*group.each
group.mu = rnorm(group.size, sd = 0.5)
eta = 6 + rep(group.mu,each = group.each)
y = rpois(n,exp(eta))

id = rep(1:group.size,each= group.each)
my.plot(id,y)
family = "poisson"
groups = rep(list(list()),n)
for(i in 1:n){
  groups[[i]] = which(id == id[i])
}
formula = y~ 1 + f(id,model = "iid", vb.correct = T, hyper = list(prec = list(initial = 0, fixed = F)))
res = inla(formula, family=family, data=list(id=id,y=y),
           inla.mode = "experimental",
           control.inla = list(control.vb = list(enable = !FALSE)),
           control.compute = list(control.gcpo = list(enable = TRUE,
                                                      group.size = 2,
                                                      epsilon = 0.005,
                                                      verbose = T),
                                  config = T),
           verbose = T)

stop("XXXXXXXXX")
## gcpo computation refit
mg = 13
rule = fastGHQuad::gaussHermiteData(mg)
ww = rule$w
xx = rule$x
A = res$misc$configs$A
res.leave.group = NA
res.refit = numeric(n)
for(point.interest in 1:n) {
    if(!all(res$gcpo$groups[[point.interest]]$idx%in%res.leave.group)){
        print(point.interest)
        res.leave.group = res$gcpo$groups[[point.interest]]$idx
        y.leave = y
        y.leave[res$gcpo$groups[[point.interest]]$idx] = NA
        res.leave = inla(formula, family=family, data=list(id=id,y=y.leave),
                         inla.mode = "experimental",
                         control.inla = list(control.vb = list(enable = !FALSE)),
                         control.compute = list(config = T),
                         control.predictor=list(link=1))
    }
    
    mu.nI = (A%*%res.leave$misc$configs$config[[1]]$improved.mean)[point.interest]
    Qinv.beta = res.leave$misc$configs$config[[1]]$Qinv + t(res.leave$misc$configs$config[[1]]$Qinv)
    diag(Qinv.beta) = 0.5* diag(Qinv.beta)
    sd.nI = sqrt(sum(A[point.interest,]*(Qinv.beta%*%A[point.interest,])))
    x.here = sqrt(2)*sd.nI*xx + mu.nI
    res.refit[point.interest] = 1/sqrt(pi)*sum(ww*dpois(x=y[point.interest],lambda = exp(x.here)))
}

ymax = max(res$gcpo$gcpo,res.refit)
my.plot(res.refit, res$gcpo$gcpo, cex = 4, pch = 19)
abline(a = 0, b = 1, lwd = 3)
