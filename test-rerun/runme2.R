data(Germany)
g <- system.file("demodata/germany.graph", package = "INLA")
source(system.file("demodata/Bym-map.R", package = "INLA"))
Germany <- cbind(Germany, region.struct = Germany$region)
Germany$x <- scale(Germany$x)

formula1 <- Y ~ 1 + 
    f(region.struct, model = "besag", graph = g, scale.model = TRUE) +
    f(region, model = "iid") + x

r <- inla(formula1, family = "poisson", data = Germany, E = E)

## add more data
rr <- inla(formula1, family = "poisson", E = E, 
          data = rbind(Germany, Germany, Germany, Germany), 
          control.mode = list(theta = r$mode$theta, restart = TRUE))

cbind(r$cpu, rr$cpu)
