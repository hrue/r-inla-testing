library(INLA)
inla.setOption(inla.mode = "experimental")
inla.setOption(num.threads = "1:1")
inla.setOption(scale.model = TRUE)
##inla.setOption(smtp = "taucs")
inla.setOption(inla.call = "inla.mkl.work")

data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))
Germany = cbind(Germany,region.struct=Germany$region)

###        RW    IID   BESAG
fixed <- c(TRUE, !TRUE, !TRUE)

##Sys.setenv(INLA_DEBUG = "gcpo")
Sys.unsetenv('INLA_DEBUG')

formula1 = Y ~ 1 +
    f(x, model="rw2", initial = 6, fixed = fixed[1], constr = T) +
    f(region, model = "iid", initial = 3, fixed = fixed[2]) +
    f(region.struct,model="besag",graph=g, initial = 3, fixed = fixed[3], constr = T) 

res1 = inla(formula1,
            family="poisson",
            E = E,
            verbose = T, 
            data=Germany,
            control.fixed = list(prec.intercept = 1),
            control.compute = list(control.gcpo = list(enable = TRUE, group.size = 2, verbose = T, epsilon = 0.001), config =T),
            control.inla = list(int.strategy = "eb", control.vb = list(enable = FALSE)))
res1 <- inla.rerun(res1)
res1 <- inla.rerun(res1)
res1 <- inla.rerun(res1)

formula2 = Y ~ 1 +
    f(region.struct,model="besag",graph=g, initial = 3, fixed = fixed[3], constr = T) +
    f(region, model = "iid", initial = 3, fixed = fixed[2]) +
    f(x, model="rw2", initial = 6, fixed = fixed[1], constr = T)

res2 = inla(formula2,
            family="poisson",
            E = E,
            verbose = T, 
            data=Germany,
            control.fixed = list(prec.intercept = 1), 
            control.compute = list(control.gcpo = list(enable = TRUE, group.size = 2, verbose = T, epsilon = 0.001), config =T),
            control.inla = list(int.strategy = "eb", control.vb = list(enable = FALSE)))
res2 <- inla.rerun(res2)
res2 <- inla.rerun(res2)
res2 <- inla.rerun(res2)

plot(res2$gcpo$gcpo, res1$gcpo$gcpo)
print(mean(abs(res1$gcpo$gcpo-res2$gcpo$gcpo)))
abline(a = 0, b = 1)

for(i in 1:length(res1$gcpo$gcpo)) {
    g1 <- res1$gcpo$groups[[i]]
    g2 <- res2$gcpo$groups[[i]]

    if (length(g1$idx) != length(g2$idx) || !all(sort(g1$idx) == sort(g2$idx))) {
        res1$gcpo$gcpo[i] <- NA
        res2$gcpo$gcpo[i] <- NA
        print(c("remove", i))
    }
}
inla.dev.new()
plot(res2$gcpo$gcpo, res1$gcpo$gcpo)
abline(a = 0, b = 1)
