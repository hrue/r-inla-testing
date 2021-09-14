n = 300
size = 1
a=0.1
loga = function(p, a=0.5) {
    return (log(p/(1-p)^a))
}

eta = seq(-12, 30, by = 0.05)
p = 1/(1+exp(-eta))
eta = loga(p, a)
eta2p = splinefun(eta, p)
p2eta = splinefun(p, eta)

x = rnorm(n)
eta = -2 + 0.2 * x
p = eta2p(eta)
y = rbinom(n, size = size, prob = p)

r = inla(y ~ 1 + x,
         control.fixed = list(prec.intercept = 1, prec = 1), 
         data = data.frame(y, x),
         family = "binomial",
         control.predictor = list(compute=TRUE), 
         control.family = list(
             control.link = list(
                 model = "loga",
                 a=1.0)), 
         Ntrials = size)
summary(r)
plot(sort(r$summary.linear.predictor$mean), lwd=2, type="l")
for(a in seq(0.99, 0.01, len=40)) {
    r$.args$control.family[[1]]$control.link$a = a
    r = inla.rerun(r)
    print(summary(r))
    lines(sort(r$summary.linear.predictor$mean), lwd=1)
}
