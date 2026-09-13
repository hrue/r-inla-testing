nx = 10^5
n.trial = 20
x = rnorm(nx)

delta = 10;
p = exp(1+x)/(1+exp(1+x))
alpha=delta*p
beta=delta*(1-p)
rb = rbeta(nx, alpha, beta, ncp = 0)

alp = 2    
q = p^alp  
y = rep(0,nx)
abs.pres = rbinom(nx,1,q)
y[abs.pres==1] = rbinom( sum(abs.pres>0), n.trial, rb[abs.pres==1])

## MAKE SURE to let data correspond to the model below, as this is all for testing that the
## results are unchanged from before

formula <- y ~ 1+x
r <- inla(formula,
          data = data.frame(x,y),
          family = "zeroinflatedbetabinomial1",
          Ntrials = rep(n.trial, nx),
          keep = TRUE,
          safe = FALSE, 
          verbose=TRUE)

