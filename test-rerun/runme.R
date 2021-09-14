data(Germany)
g <- system.file("demodata/germany.graph", package = "INLA")
source(system.file("demodata/Bym-map.R", package = "INLA"))
Germany <- cbind(Germany, region.struct = Germany$region)
Germany$x <- scale(Germany$x)
Germany$intercept <- 1
formula1 <- Y ~ -1 + intercept +
    f(region.struct, model = "besag", graph = g, scale.model = TRUE) +
    f(region, model = "iid") + x
r <- inla(formula1,
          family = "poisson",
          data = Germany, 
          E = E,
          control.predictor = list(compute = TRUE), 
          control.compute = list(config = TRUE))
rr <- inla(formula1,
           family = "poisson",
           data = Germany, 
           E = E,
           control.predictor = list(compute = TRUE), 
           control.mode = list(result = r, restart = TRUE), 
           control.compute = list(config = TRUE))

print(as.numeric((object.size(rr)-object.size(r))/object.size(r) * 100))
for(n in sort(names(r))) {
    m1 <- object.size(r[[n]])
    m2 <- object.size(rr[[n]])
    if (m1 != m2)
        print(c(n, m1, m2))
}
