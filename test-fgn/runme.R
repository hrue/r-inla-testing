library(FGN)
n = 1000
H = 0.85
y = SimulateFGN(n, H)
y = y - mean(y)
r = inla(y ~ -1 + f(idx, model="fgn", order=3),
         data = data.frame(y, idx=1:n),
         control.compute = list(smtp="band"),
         control.inla = list(reordering = "band"), 
         control.family =list(hyper = list(prec = list(initial = 12, fixed=TRUE))))
rr = inla(y ~ -1 + f(idx, model="fgn", order=4),
         data = data.frame(y, idx=1:n),
         control.compute = list(smtp="band"),
         control.inla = list(reordering = "band"), 
         control.family =list(hyper = list(prec = list(initial = 12, fixed=TRUE))))
print(c(MLE=FitFGN(y, demean=TRUE)$H,
        Post.mean3=r$summary.hyperpar[2,"mean"],
        Post.mode3=r$summary.hyperpar[2,"mode"], 
        Post.mean4=rr$summary.hyperpar[2,"mean"],
        Post.mode4=rr$summary.hyperpar[2,"mode"]))

