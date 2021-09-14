n = 500

x = rnorm(n, sd = 0.5)
mu = 0 + x
poffset = 0.2
size = sample(1:10, n, replace=TRUE)
prob = poffset + (1-poffset) * 1/(1 + exp(-mu))
y = rbinom(n, prob = prob, size = size)

r = inla(y  ~ -1 + x,
         data = data.frame(y, x, size),
         Ntrials = size,
         verbose=TRUE,
         family = "binomial", 
         control.family = list(
             control.link = list(
                 model = "logitoffset",
                 hyper = list(
                     prob = list(
                         prior = "normal",
                         param = c(-1, 0.1))))))


