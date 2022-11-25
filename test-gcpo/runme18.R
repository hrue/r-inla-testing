library(INLA)
INLA:::inla.my.update()
inla.setOption(num.threads = "1:1")
inla.setOption(inla.call = "inla.mkl.work")

data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))
summary(Germany)

n <- nrow(Germany)
Germany$id <- 1:nrow(Germany)
formula3 = Y ~ f(region, model="besag", graph=g) + f(id) + f(x, model="rw2")

r = inla(formula3,
         family="poisson",
         data=Germany,
         safe = FALSE, 
         control.inla = list(int.strategy = "eb"), 
         E=E,
         verbose = !TRUE)

friends <- vector('list', n)
for(i in 1:n) {
    if (i > 1 && i < n)
        friends[[i]] <- c(i-1, i+1)
}
r1 <- inla.group.cv(r, strategy = "posterior", num.level.sets = 3, friends = friends, verbose = !TRUE)

## validate that we get the same doing it 'manually'
r2 = inla(formula3,
         family="poisson",
         data=Germany,
         safe = FALSE, 
         control.inla = list(int.strategy = "eb"),
         control.compute = list(control.gcpo = list(
                                    enable = TRUE, 
                                    strategy = "posterior",
                                    num.level.sets = 3,
                                    friends = friends,
                                    verbose = !TRUE)), 
         E=E,
         verbose = !TRUE)
