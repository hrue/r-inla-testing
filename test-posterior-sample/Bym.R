data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))

n <- nrow(Germany)
idx.pred <- sort(sample(1:n, 25, replace = FALSE))
Germany$Y[idx.pred] <- NA
formula <- Y ~ f(region, model="bym2", graph=g) + x
r <- inla(formula, family="poisson", data=Germany, E=E,
          control.predictor=list(link=1), 
          control.compute = list(config = TRUE),
          inla.mode = "experimental", 
          verbose = T)

xx <- inla.posterior.sample(1000, r)
yy <- inla.posterior.sample.eval(
    function(idx, E, q) qpois(q, lambda = E[idx] * exp(Predictor[idx])), 
    xx, return.matrix = TRUE,
    idx = idx.pred, E = Germany$E, q = 0.9)
q.est <- rowMeans(yy)
