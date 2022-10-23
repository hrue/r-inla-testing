library(INLA)
INLA:::inla.my.update()
inla.setOption(inla.mode = "experimental")
inla.setOption(num.threads = "1:1")
inla.setOption(inla.call = "inla.mkl.work")

data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))
summary(Germany)

##Germany <- rbind(Germany, Germany, Germany, Germany)
##Germany <- rbind(Germany, Germany, Germany, Germany)
##Germany <- rbind(Germany, Germany, Germany, Germany)

Germany$id <- 1:nrow(Germany)
formula3 = Y ~ f(region, model="besag", graph=g) + f(id) + f(x, model="rw2")

Sys.setenv(INLA_TRACE="GMRFLib_gcpo,GMRFLib_gcpo_build")
Sys.unsetenv("GCPO_NEW")
if (!exists("r")) {
r = inla(formula3,
         family="poisson",
         data=Germany,
         ##control.compute = list(control.gcpo = list(enable = TRUE)), 
         safe = FALSE, 
         E=E,
         verbose = TRUE)
}
t1<- Sys.time()
r1 <- inla.group.cv(r, num.level.sets = 2)
t1 <- Sys.time() - t1

Sys.setenv(GCPO_NEW = 1)
if (!exists("rr")) {
    rr = inla(formula3,
              family="poisson",
              data=Germany,
              ##control.compute = list(control.gcpo = list(enable = TRUE)), 
              safe = FALSE, 
              E=E,
              verbose = TRUE)
}

tt1 <- Sys.time()
rr1 <- inla.group.cv(rr, num.level.sets = 2)
tt1 <- Sys.time() - tt1

print(mean(abs(rr1$cv - r1$cv)))
print(c(t1, tt1, t1-tt1))

