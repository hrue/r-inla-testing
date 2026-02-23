library(INLA)

INLA:::inla.my.update(b = T)
inla.setOption(scale.model = TRUE)
inla.setOption(keep = TRUE)
inla.setOption(num.threads = "8:2:4")
inla.setOption(inla.call = "inla.mkl.work")

data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))
##summary(Germany)

## just make a duplicated column
Germany = cbind(Germany,region.struct=Germany$region)

formula3 = Y ~ 1 +
    f(region.struct, model="besag", graph=g,
      diagonal = 1e-3, constr = T, initial = 3.5, fixed = TRUE) + 
    f(region,model="iid", initial = 9, fixed = TRUE) +
    f(x, model="rw2", constr = T, diagonal = 1e-3, initial = 9, fixed = TRUE)

r.stiles = inla(formula3,
                family="poisson",
                data=Germany, 
                E=E,
                safe = FALSE, 
                verbose = TRUE,
                keep = TRUE, 
                control.fixed = list(
                    prec.intercept = 1,
                    prec = 1), 
                control.inla = list(
                    int.strategy = "eb",
                    control.vb = list(verbose = TRUE)), 
                control.compute = list(
                    cpo = TRUE, 
                    smtp = "stiles", 
                    control.gcpo = list(
                        enable = TRUE, 
                        num.level.sets = 5,
                        size.max = 10, 
                        verbose = !TRUE)))

r.taucs = inla(formula3,
                family="poisson",
                data=Germany, 
                E=E,
                safe = FALSE, 
                verbose = TRUE,
                keep = TRUE, 
                control.fixed = list(
                    prec.intercept = 1,
                    prec = 1), 
                control.inla = list(
                    int.strategy = "eb",
                    control.vb = list(verbose = TRUE)), 
                control.compute = list(
                    cpo = TRUE, 
                    smtp = "taucs", 
                    control.gcpo = list(
                        enable = TRUE, 
                        num.level.sets = 5,
                        size.max = 10, 
                        verbose = !TRUE)))

rr.stiles <- inla.group.cv(r.stiles, groups = r.stiles$gcpo$groups)
rr.taucs <- inla.group.cv(r.taucs, groups = r.taucs$gcpo$groups)

par(mfrow = c(1, 3))
plot(r.stiles$gcpo$gcpo,  rr.stiles$cv)
abline(a = 0, b = 1)
print(mean(abs(r.stiles$gcpo$gcpo - rr.stiles$cv)))

plot(r.taucs$gcpo$gcpo,  rr.taucs$cv)
abline(a = 0, b = 1)
print(mean(abs(r.taucs$gcpo$gcpo - rr.taucs$cv)))

plot(r.stiles$gcpo$gcpo,  r.taucs$gcpo$gcpo)
abline(a = 0, b = 1)
print(mean(abs(r.stiles$gcpo$gcpo - r.taucs$gcpo$gcpo)))

r <- r.stiles
rr <- rr.stiles

for(i in 1:544) {
    if (!is.na(r$gcpo$gcpo[i]) && !is.na(rr$cv[i]) && (abs(r$gcpo$gcpo[i] - rr$cv[i]) > 0.001)) {
        cat("i=", i, "\n")
        cat("idx ")
        print(c(r$gcpo$groups[[i]]$idx, rr$groups[[i]]$idx))
        cat("corr ")
        print(round(dig = 5, c(r$gcpo$groups[[i]]$corr, rr$groups[[i]]$corr)))
        cat("gcpo/cv " )
        print(round(dig = 15, c(r$gcpo$gcpo[i], rr$cv[i]))) 
    }
}
