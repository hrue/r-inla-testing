n = 100
x = rnorm(n, sd = 1)
eta = 1.1 + 0.77*x
pp = 1.0/(1+exp(-eta))
alpha = 0.95
ntrials = sample(1:5,  size=n, replace=TRUE)
p = qbeta(alpha, ntrials*pp + 1,  ntrials*(1-pp), lower.tail=FALSE)
y = rbinom(n = n, size=ntrials, prob = p)
r = inla(y ~ 1 + x,
         family = "xbinomial",
         control.family = list(list(
             control.link = list(
                 model = "quantile",
                 quantile = alpha))), 
         Ntrials = ntrials, 
         data = data.frame(y, x), 
         control.predictor = list(compute=TRUE), 
         verbose=TRUE)

