n = 300
Ntrials = 10
x = rnorm(n, sd = 1)
eta = 1 + x
alpha = 1^3
prob = inla.link.invsn(eta, a = alpha)
y = rbinom(n, size = Ntrials,  prob = prob)

r = inla(y ~ 1 + x,
         family = "binomial",
         data = data.frame(y, x),
         Ntrials = Ntrials,
         verbose = TRUE, 
         ##control.inla = list(int.strategy = "ccd"), 
         control.inla = list(int.strategy = "grid"), 
         control.family = list(
             control.link = list(
                 model = "sn",
                 hyper = list(alpha = list(
                                  prior = "pc.sn", 
                                  initial = 0,
                                  fixed=FALSE)))))
summary(r)
plot(r, plot.prior=TRUE)

