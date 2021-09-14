require(INLA)
library(evd)

giveme.mu = function(q, sbeta, alpha, beta, xi){
  a = -log(1-beta/2)
  b = -log(beta/2)
  c = -log(alpha)
  if(xi != 0){
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
giveme.sigma = function(q, sbeta, alpha, beta, xi){
  a = -log(1-beta/2)
  b = -log(beta/2)
  if(xi != 0){
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
x = rnorm(n)
y = rnorm(n)
z = rnorm(n)
eta = 1 + x # the linear predictor
s.x = 0.2*eta + 0.5*y # covariates for the spread parameter
xi.x = 0.1 # shape parameter fixed
# xi.x = 1 + 0.2*z # alternatively, we can putcovariates in the shape parameter
alpha = 0.5
beta = 0.05

mu = giveme.mu(q = exp(eta),
               sbeta = exp(s.x), # since the spread should be positive
               alpha = alpha,
               beta = beta,
               xi = xi.x)

sigma = giveme.sigma(q = exp(eta),
                     sbeta = exp(s.x),
                     alpha = alpha,
                     beta = beta,
                     xi = xi.x)


dat = rgev(n, loc = mu, scale = sigma, shape = xi.x)

