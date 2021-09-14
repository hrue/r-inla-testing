
jp.func = function(theta) {
    ## return the sum of all log-priors on the theta-scale. the 'theta[1]' is the log
    ## jacobian...
    lprior = 0
    for(k in seq_along(param)) 
        lprior = lprior +
            (dgamma(exp(theta[k]), shape = param[[k]][1], rate = param[[k]][2], log=TRUE) +
             theta[k])

    ## print(paste("call 'jp' with theta: ", theta, " return ", lprior, sep="",  collapse=" "))
    return (lprior)
}


param = list(
    param.1 = c(1, 2), 
    param.2 = c(30, 4), 
    param.3 = c(5, 60), 
    param.4 = c(70, 8))


n = 100
m = c(n, 20, 10, 5)
y = rnorm(n)
iid1 = inla.rgeneric.define(inla.rgeneric.iid.model, n=m[1], param = param[[1]])
iid2 = inla.rgeneric.define(inla.rgeneric.iid.model, n=m[2], param = param[[2]])
iid3 = inla.rgeneric.define(inla.rgeneric.iid.model, n=m[3], param = param[[3]])
iid4 = inla.rgeneric.define(inla.rgeneric.iid.model, n=m[4], param = param[[4]])

jp = inla.jp.define(jp.func, param = param)

r = inla(y ~ -1 + f(idx1, model=iid1) + f(idx2, model=iid2) + f(idx3, model=iid3) + f(idx4, model=iid4), 
         data = data.frame(y,
                           idx1 = rep(1:m[1], n)[1:n], 
                           idx2 = rep(1:m[2], n)[1:n], 
                           idx3 = rep(1:m[3], n)[1:n], 
                           idx4 = rep(1:m[4], n)[1:n]),
         control.family = list(hyper = list(prec = list(initial = 10, fixed=TRUE))), 
         verbose=T,
         control.compute = list(openmp.strategy = "huge"), 
         control.expert = list(jp = jp))
r = inla.rerun(r)

rr = inla(y ~ -1 +
              f(idx1, hyper = list(prec = list(param = param[[1]]))) +
              f(idx2, hyper = list(prec = list(param = param[[2]]))) +
              f(idx3, hyper = list(prec = list(param = param[[3]]))) +
              f(idx4, hyper = list(prec = list(param = param[[4]]))), 
         data = data.frame(y, 
                           idx1 = rep(1:m[1], n)[1:n], 
                           idx2 = rep(1:m[2], n)[1:n], 
                           idx3 = rep(1:m[3], n)[1:n], 
                           idx4 = rep(1:m[4], n)[1:n]),
         control.family = list(hyper = list(prec = list(initial = 10, fixed=TRUE))))
rr = inla.rerun(rr)

r$mlik - rr$mlik



