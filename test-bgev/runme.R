require(INLA)
library(evd)

giveme.mu = function(q, sbeta, alpha, beta, xi)
{
    a = -log(1-beta/2)
    b = -log(beta/2)
    c = -log(alpha)
    if(xi[1] != 0){
        tmp0 = (c^(-xi) - 1)/xi
        tmp1 = a^(-xi)
        tmp2 = b^(-xi)
        dbeta = (tmp1 - tmp2)/xi
        return(q - (sbeta/dbeta) * tmp0)
    }else{
        dbeta = log(b) - log(a)
        tmp0 = log(c)
        return(q + (sbeta/dbeta) * tmp0)
    }
    
}

giveme.sigma = function(q, sbeta, alpha, beta, xi)
{
    a = -log(1-beta/2)
    b = -log(beta/2)
    if(xi[1] != 0){
        tmp1 = a^(-xi)
        tmp2 = b^(-xi)
        dbeta = (tmp1 - tmp2)/xi
        return(sbeta/dbeta)
    }else{
        dbeta = log(b) - log(a)
        return(sbeta/dbeta)
    }
    
}

n = 1000
x = rnorm(n, sd = 0.01)
y = rnorm(n, sd = 0.01)
z = rnorm(n, sd = 0.01)

y[] = 0
z[] = 0
##x[] = 0

eta = 1 + x
spread = exp(eta)
xi = 0.25
location = 1 + 0.1 * y
xi.x = rep(xi, n) * exp(0.0 * z)
alpha = 0.5
beta = 0.05

if (all(x == 0)) x = matrix(0, n, 0)
if (all(y == 0)) y = matrix(0, n, 0)
if (all(z == 0)) z = matrix(0, n, 0)

mu = giveme.mu(q = location,
               sbeta = spread, 
               alpha = alpha,
               beta = beta,
               xi = xi.x)

sigma = giveme.sigma(q = location,
                     sbeta = spread, 
                     alpha = alpha,
                     beta = beta,
                     xi = xi.x)

yy = numeric(n)
for(i in 1:n) {
    yy[i] = rgev(1, loc = mu[i], scale = sigma[i], shape = xi.x[i])
}

r = inla(inla.mdata(yy, y) ~ offset(off) + 1 + x, 
         family = "gev2",
         control.family = list(
             gev2.scale.xi = 0.01, 
             hyper = list(theta1 = list(initial=0, fixed=T, param = c(0, 1000)),
                          theta2 = list(param = c(0, 0.1)), 
                          loc = list(initial = min(yy), 
                                     fixed = FALSE), 
                          tail = list(initial = log(0.1),
                                      fixed = FALSE,
                                      prior = "loggamma",
                                      param = c(1, 2)))), 
         control.fixed = list(prec.intercept = 1), 
         control.compute = list(smtp = "pardiso"), 
         data = data.frame(yy, off=-1, x),
         control.inla = list(cmin = 0, h=0.01), 
         verbose=TRUE)
