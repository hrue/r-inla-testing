library(INLA)
m <- inla.models()
m <- get("inla.models", env = INLA:::inla.get.inlaEnv())
m$link$sslogit$status <- NULL
assign("inla.models", m, env = INLA:::inla.get.inlaEnv())
rm(m)

n = 50
size = 1
x = rnorm(n, s=0.2)
eta = 1 + x + 0.5 * scale(arima.sim(n, model = list(ar = 0.9)))
sens = 0.9
spec = 0.76
fixed = TRUE
p = 1/(1+exp(-eta))
prob = sens * p + (1-spec) * (1 - p)
y = rbinom(n, size=size,  prob=prob)

a = 100
b = function(s, a) a*(1-s)/s

r = inla(y ~ 1 + x + f(idx, model = "ar1",
                       hyper = list(
                           prec = list(prior = "pc.prec",
                                       param = c(1, 0.01)),
                           rho = list(prior = "pc.cor1",
                                      initial = 5, 
                                      param = c(0.83, 0.5)))), 
         data = data.frame(y, x, size, idx = 1:n),
         family = "binomial",
         Ntrials = size,
         verbose = TRUE,
         inla.mode = "experimental", 
         ##control.inla = list(cmin = Inf, b.strategy = "keep"), 
         ##control.inla = list(cmin = -Inf, b.strategy = "keep"), 
         control.family = list(
             control.link = list(
                 model = "sslogit",
                 hyper = list(
                     sens = list(
                         initial = inla.link.logit(sens),
                         fixed = TRUE), 
                     spec = list(
                         initial = inla.link.logit(spec),
                         fixed = TRUE)))))

rr = inla(y ~ 1 + x + f(idx, model = "ar1",
                        hyper = list(
                            prec = list(prior = "pc.prec",
                                        param = c(1, 0.01)),
                            rho = list(prior = "pc.cor1",
                                       initial = 5, 
                                       param = c(0.83, 0.5)))), 
          data = data.frame(y, x, size, idx = 1:n),
          family = "binomial",
          Ntrials = size,
          verbose = TRUE,
          inla.mode = "experimental", 
          control.inla = list(cmin = Inf, b.strategy = "keep"), 
          control.family = list(
              control.link = list(
                  model = "sslogit",
                  hyper = list(
                      sens = list(
                          initial = inla.link.logit(sens),
                          fixed = TRUE), 
                      spec = list(
                          initial = inla.link.logit(spec),
                          fixed = TRUE)))))
