library(INLA)

data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))
summary(Germany)

Germany = cbind(Germany,region.struct=Germany$region)
form = Y ~ f(region.struct,model="besag",graph=g) +
               f(region,model="iid") + f(x, model="rw2")
r =  inla(form,
          family="poisson",
          data=Germany,
          E=E, safe = F, 
          verbose = !TRUE, 
          control.inla = list(int.strategy = "eb", control.vb = list(enable = FALSE), verbose = !TRUE), 
          control.predictor = list(compute = TRUE), 
          control.compute = list(config = TRUE))

for (i in 1:5) {
    rr <- inla.rerun(r)
    print(round(dig = 5, c(as.numeric(rr$mode$theta - r$mode$theta),
                           mean(abs(rr$mode$x - r$mode$x)),
                           rr$mode$log.posterior.mode-r$mode$log.posterior.mode)))
                           
    r <- rr
}

