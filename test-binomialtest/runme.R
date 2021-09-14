inla.my.update(b=T)

trans = function(x, psi=1) {

    idx = which(x >= 0)
    x[idx] = ((x[idx] + 1)^psi -1)/psi
    idx = which(x < 0)
    x[idx] = -((-x[idx] + 1)^psi -1)/psi

    return (x)
}
    

n = 10000
size = sample(c(1, 5, 10),  n, replace=TRUE)
z = rnorm(n, sd=0.2)
psi = 1.25
eta = trans(1 + z, psi)
p = 1/(1+exp(-eta))
y = rbinom(n, prob=p, size=size)
formula = y ~ 1 + z

if (FALSE) {
    ## just to check with binomial if psi=1
    rr = inla(formula,  data = data.frame(y, z, size), 
            family = "binomial", Ntrials = size,  verbose=TRUE,
            control.data = list(hyper = list(theta = list(fixed=TRUE))))
}

prior.psi = "normal"
param.psi = c(0, 1)

r = inla(formula,  data = data.frame(y, z, size), 
        family = "binomialtest",
        Ntrials = size,  
        control.data = list(
                hyper = list(
                        psi = list(
                                prior = prior.psi,
                                param = param.psi
                                )
                        )
                ), 
        verbose=TRUE)

if (FALSE) {
    ## plot the prior for psi
    xx = rnorm(100000, mean=param.psi[1], sd = 1/sqrt(param.psi[2]))
    plot(density( 0.5 + 1/(1 +exp(-xx))), main = "prior for psi")
}
