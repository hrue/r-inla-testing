n = 25
if (!exists("y")) {
    x <- rnorm(n, sd = 0.3)
    eta = 2 + x
    y = rpois(n, exp(eta))
}

inla.setOption(inla.call = "inla.mkl.work")

r = inla(y ~ 1 + x, 
         family = "poisson", 
         control.fixed = list(mean.intercept = log(mean(y)), prec.intercept = 0.001), 
         data = data.frame(y, x), 
         control.predictor = list(hyper = list(prec = list(initial = 25))), 
         control.compute = list(dic = TRUE, po = TRUE), 
         control.inla = list(strategy = "gaussian"), 
         inla.mode = "classic",
         verbose = T)

rr = inla(y ~ 1 + x, 
         family = "poisson", 
         control.fixed = list(mean.intercept = log(mean(y)), prec.intercept = 0.001), 
         data = data.frame(y, x), 
         control.compute = list(dic = TRUE, po = TRUE), 
         control.inla = list(strategy = "gaussian"), 
         inla.mode = "experimental",
         verbose = T)

head(cbind(classic = r$dic$local.dic - r$dic$local.p.eff,
           experimental = rr$dic$local.dic - rr$dic$local.p.eff, 
           plugin = -2*dpois(y, lambda = mean(y), log = TRUE)))

head(cbind(sat.classic = r$dic$local.dic.sat - r$dic$local.p.eff,
           sat.experimental = rr$dic$local.dic.sat - rr$dic$local.p.eff,
           sat.plugin = -2*(dpois(y, lambda = mean(y), log = TRUE) - dpois(y, lambda = y, log = TRUE))))


r$summary.fixed
rr$summary.fixed
