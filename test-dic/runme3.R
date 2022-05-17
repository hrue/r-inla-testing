n = 1
eta = 1
y = rpois(n, exp(eta))
y <- c(100)
r.ref = inla(y ~ 1, 
         family = "poisson", 
         ##family = "normal", 
         data = data.frame(y), 
         control.compute = list(dic = TRUE, waic = TRUE, smtp = "band"),
         inla.mode = "experimental", 
         num.threads = "1:1",
         inla.call = "inla.mkl")
rg= inla(y ~ 1, 
         family = "poisson", 
         ##family = "normal", 
         data = data.frame(y), 
         control.compute = list(dic = TRUE, waic = TRUE, smtp = "band"),
         control.inla = list(strategy = "gaussian"), 
         num.threads = "1:1",
         inla.call = "inla.mkl.work")
r = inla(y ~ 1, 
         family = "poisson", 
         ##family = "normal", 
         data = data.frame(y), 
         control.compute = list(dic = TRUE, waic = TRUE, smtp = "band"),
         control.inla = list(strategy = "simplified.laplace"), 
         num.threads = "1:1",
         inla.call = "inla.mkl.work")
summary(r)
cbind(r.ref$dic$local.dic, rg$dic$local.dic, r$dic$local.dic)

