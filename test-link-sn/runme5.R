n = 200000
Ntrials = 2
x = rnorm(n, sd = 0.5)
eta = x
skew <- 0.0
prob = inla.link.invsn(eta, skew = skew, intercept = 0.75)
y = rbinom(n, size = Ntrials,  prob = prob)

r = inla(y ~ 1 + x,
         family = "binomial",
         data = data.frame(y, x),
         Ntrials = Ntrials,
         verbose = TRUE,
         keep = TRUE, 
         control.fixed = list(remove.names = "(Intercept)",
                              prec = 1), 
         control.family = list(
             control.link = list(
                 model = "sn",
                 hyper = list(
                     skew = list(initial = 0,
                                 fixed = TRUE),
                     intercept = list(param = c(0, 1))))),
         twostage = TRUE)

rr = inla(y ~ 1 + x,
         family = "binomial",
         data = data.frame(y, x),
         Ntrials = Ntrials,
         control.fixed = list(prec = 1, prec.intercept = 1), 
         control.family = list(
             control.link = list(
                 model = "probit")), 
         verbose = TRUE)

r$mlik - rr$mlik

