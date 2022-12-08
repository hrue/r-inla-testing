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
r1 <- inla.group.cv(r, strategy = "posterior", num.level.sets = -1, friends = friends, groups = NULL, verbose = TRUE)

groups <- vector('list', n)
for(i in 1:n) {
    if (i > 1 && i < n)
        groups[[i]] <- c(i-1, i+1)
}
groups[[1]] <- 1
groups[[n]] <- n

r2 <- inla.group.cv(r, strategy = "posterior", groups = groups, verbose = TRUE)
