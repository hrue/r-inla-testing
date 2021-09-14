pqbin = function(q, n, quantile.level) {
    return (qbeta(quantile.level, n*q + 1, n*(1-q),
                  lower.tail = FALSE, log.p = FALSE))
}
qbin = function(q, quantile.level){
    return (pqbin(q, 1, quantile.level))
}

n = 1000
x = rnorm(n, sd = 0.5)
x = x - mean(x)
eta = 1 + x
q = 1/(1+exp(-eta))
Ntrials = sample(1:10, n, replace=T)
quantile.level = 0.8

p = qbin(q, quantile.level)
y = rbinom(n, prob=p, size=Ntrials)
r = inla(y ~ 1 + x,
         data = data.frame(y, x, Ntrials), 
         Ntrials = Ntrials,
         family = "binomial",
         control.family = list(
             control.link = list(model="quantile",
                                 quantile = quantile.level)))
summary(r)

p = pqbin(q, Ntrials, quantile.level)
y = rbinom(n, prob=p, size=Ntrials)
r = inla(y ~ 1 + x,
         data = data.frame(y, x, Ntrials), 
         Ntrials = Ntrials,
         family = "binomial",
         control.family = list(
             control.link = list(model="pquantile",
                                 quantile = quantile.level)))
summary(r)
