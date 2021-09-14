nx = 1000;                       # number of x values
n.trial = 20                     # size of each binomial trial
x = rnorm(nx)           # generating some random data

delta = 10;
p = exp(1+x)/(1+exp(1+x))
alpha=delta*p
beta=delta*(1-p)
rb = rbeta(nx, alpha, beta, ncp = 0)

alp = 2                  #ZI parameter
q = p^alp                        #prob presence
y = rep(0,nx)
abs.pres = rbinom(nx,1,q)
y[abs.pres==1] = rbinom( sum(abs.pres>0), n.trial, rb[abs.pres==1])

if (FALSE) {
    inla.my.update()
    inla.setOption("inla.call", "inla.work")
}

formula = y ~ x +1
r = inla(formula, data = data.frame(x,y), family = "zeroinflatedbetabinomial2",
        control.data = list(prior = c("flat", "flat"),
                fixed = c(F,F)),
        Ntrials = rep(n.trial, nx),
        verbose=TRUE)

