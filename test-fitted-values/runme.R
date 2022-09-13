 r=inla(y~1,
        data=data.frame(y=1),
        family = "poisson", 
        control.predictor=list(compute=TRUE, link=1),
        control.compute=list(return.marginals.predictor=TRUE))
r$summary.fitted.values
r$summary.linear.predictor

e.1 <- inla.emarginal(function(x) exp(x), r$marginals.linear.predictor[[1]])
e.2 <- inla.emarginal(function(x) x, r$marginals.fitted.values[[1]])
ee.1 <- inla.emarginal(function(x) exp(2*x), r$marginals.linear.predictor[[1]])
ee.2 <- inla.emarginal(function(x) x^2, r$marginals.fitted.values[[1]])

print(c(ee.1 - e.1^2,  ee.2 - e.2^2))

