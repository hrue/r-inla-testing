n = 200
Ntrials = 200
x = rnorm(n, sd = 0.5)
eta = x
skew <- 0.5
prob = inla.link.invsn(eta, skew = skew, intercept = 0.75)
y = rbinom(n, size = Ntrials,  prob = prob)

r = inla(y ~ 1 + x,
         family = "binomial",
         data = data.frame(y, x),
         Ntrials = Ntrials,
         verbose = TRUE,
         control.fixed = list(remove.names = "(Intercept)", prec = 1), 
         control.family = list(
             control.link = list(
                 model = "sn",
                 hyper = list(skew = list(
                                  prior = "pc.sn",
                                  initial = 0.01, 
                                  param = 5, 
                                  fixed=FALSE),
                              intercept = list(
                                  fixed = FALSE)))))
summary(r)
plot(r,  plot.prior = TRUE)

rr = inla(y ~ 1 + x,
         family = "binomial",
         data = data.frame(y, x),
         Ntrials = Ntrials,
         verbose = TRUE,
         control.fixed = list(prec = 1), 
         control.family = list(
             control.link = list(
                 model = "probit")))

r$mlik
rr$mlik
