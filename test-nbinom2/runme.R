n = 10000
x = rnorm(n, sd = 0.2)
eta = 1 + 1.1*x 
p = exp(eta)/(1 + exp(eta))
size = sample(100:500, n, replace=TRUE)
y = rnbinom(n, size = size, prob = p)
r = inla(y ~ 1 + x + f(idx), family = "nbinomial2", Ntrials = size,
         data = data.frame(y, x, size, idx = 1:n))
summary(r)

if (TRUE) {
    ## this is for testing only
    print(r$mlik)
    for(i in 1:1) {
        rr = (inla(y ~ 1 + x + f(idx), family = "nbinomial2", Ntrials = size,
                   data = data.frame(y, x, size, idx = 1:n), num.threads = 1, inla.call =
                                                                                  'inla.work'))
        print(c(rr$cpu,  rr$mlik))
    }
}
