n = 3000
Ntrials = 200
x = rnorm(n, sd = .5)
eta = 1+x
skew <- 0.6
prob = inla.link.invsn(eta, skew = skew, intercept = 0.5)
y = rbinom(n, size = Ntrials,  prob = prob)

r = inla(y ~ -1 + x,
         family = "binomial",
         data = data.frame(y, x),
         Ntrials = Ntrials,
         verbose = TRUE,
         control.inla = list(int.strategy = "eb", cmin = 0), 
         control.family = list(
             control.link = list(
                 model = "sn",
                 hyper = list(skew = list(
                                  prior = "pc.sn",
                                  param = 20, 
                                  fixed=FALSE),
                              intercept = list(
                                  initial = 0)))))
summary(r)
plot(r,  plot.prior = TRUE)

r <- inla.hyperpar(r)
plot(r,  plot.prior = TRUE)
