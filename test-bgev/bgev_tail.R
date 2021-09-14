library(INLA)
library(evd)

rm(list=ls())
giveme.gev.par = function(q, sbeta, alpha, beta, xi) 
{
  .mu = function(q, sbeta, alpha, beta, xi) { 
    a = -log(1-beta/2)
    b = -log(beta/2)
    c = -log(alpha)
    if (all(xi > 0.0)) {
      tmp0 = (c^(-xi) - 1)/xi
      tmp1 = a^(-xi)
      tmp2 = b^(-xi)
      dbeta = (tmp1 - tmp2)/xi 
      return(q - (sbeta/dbeta) * tmp0)
    } else if (all(xi == 0.0)) { 
      dbeta = log(b) - log(a) 
      tmp0 = log(c)
      return(q + (sbeta/dbeta) * tmp0) 
    } else {
      stop("mixed case not implemented") }
  }
  
  .sigma = function(q, sbeta, alpha, beta, xi) { 
    a = -log(1-beta/2)
    b = -log(beta/2)
    if (all(xi > 0.0)) {
      tmp1 = a^(-xi)
      tmp2 = b^(-xi)
      dbeta = (tmp1 - tmp2)/xi 
      return(sbeta/dbeta)
    } else if (all(xi == 0.0)) { 
      dbeta = log(b) - log(a) 
      return(sbeta/dbeta)
    } else {
      stop("mixed case not implemented")
    } 
  }
  
  return(list(mu = .mu(q, sbeta, alpha, beta, xi),
              sigma = .sigma(q, sbeta, alpha, beta, xi),
              xi = xi))
}

map.tail = function(x, interval, inverse = FALSE) { 
  if (!inverse) {
    return (interval[1] + (interval[2] - interval[1]) * exp(x)/(1.0 + exp(x))) 
  } else {
    return (log((x-interval[1])/(interval[2]-x))) 
  }
}

## hyperpar definition 
p.alpha = 0.5
p.beta = 0.25
tail.interval = c(0, 0.5)




## Example   ==================
## here median and spread are fixed and the only parameter that has one covariate is the tail


####### PARAMETERS ========================================================================
## spread and median are fixed
n = 1000

eta.x = rep(1,n)
spread = rep(exp(-1),n)

## the tail has a covariate
x.tail =rnorm(n)

## linear predictor for tail parameter
alpha.tail = -0.5
beta.tail1 = 1.5
beta.tail2 = -0.5

eta.tail1 = alpha.tail + beta.tail1 * x.tail
eta.tail2 = alpha.tail + beta.tail2 * x.tail
## get the tail parameter using the mapping function (this is in the chosen inteval)
tail1 = map.tail(eta.tail1, tail.interval, inverse = FALSE)
tail2 = map.tail(eta.tail2, tail.interval, inverse = FALSE)

par1 = giveme.gev.par(q = eta.x,
                     sbeta = spread,
                     alpha = p.alpha,
                     beta = p.beta,
                     xi = tail1)
par2 = giveme.gev.par(q = eta.x,
                      sbeta = spread,
                      alpha = p.alpha,
                      beta = p.beta,
                      xi = tail2)


####### SIMULATE DATA =======================================
y1 = y2 = numeric(n)
for(i in 1:n)
{
  y1[i] = rgev(1, loc = par1$mu[i], scale = par1$sigma[i], shape = par1$xi[i])
  y2[i] = rgev(1, loc = par2$mu[i], scale = par2$sigma[i], shape = par2$xi[i])
}


null.matrix = matrix(nrow = n, ncol= 0)
spread.x = null.matrix
tail.x = x.tail

data1.bgev = data.frame(y = y1, 
                       intercept = 1,
                       spread.x = spread.x,
                       tail.x = tail.x)
data2.bgev = data.frame(y = y2, 
                        intercept = 1,
                        spread.x = spread.x,
                        tail.x = tail.x)

formula = inla.mdata(y, spread.x, tail.x) ~ -1 + intercept
r1 = inla(formula,
          family = "bgev",
          data = data1.bgev,
          control.family = list(hyper = list(
                                    beta1 = list(prior = "normal",
                                                 param = c(0, 4)))), 
          control.compute=list(config = TRUE),
          control.inla = list(int.strategy = "grid"),
          verbose=TRUE)

summary(r1)

r2 = inla(formula,
          family = "bgev",
          data = data2.bgev,
          control.family = list(hyper = list(
                                    beta1 = list(prior = "normal",
                                                 param = c(0, 4)))), 
          control.compute=list(config = TRUE),
          control.inla = list(int.strategy = "grid"),
          verbose=TRUE)

summary(r2)



print(c(alpha.tail, beta.tail1, beta.tail2))
r1$internal.summary.hyperpar
r2$internal.summary.hyperpar




## try to predict values for the tail parameter (use a plug in instead of inla.posterior.sample)
## this is the linear predictot
est.eta.tail1 = r1$internal.summary.hyperpar[2,1] + r1$internal.summary.hyperpar[3,1] * tail.x
est.eta.tail2 = r2$internal.summary.hyperpar[2,1] + r2$internal.summary.hyperpar[3,1] * tail.x


est.tail1  = map.tail(est.eta.tail1, tail.interval, inverse = FALSE)
est.tail2  = map.tail(est.eta.tail2, tail.interval, inverse = FALSE)

## do the same but more correctly using inla.posterior.samples


samples1 = inla.posterior.sample(1000, r1)
samples2 = inla.posterior.sample(1000, r2)


func_tail = function(..., x = x.tail)
{
  eta = map.tail(theta[2],c(0,0.5),inverse = T) + theta[3] * x
  return(map.tail(eta, c(0,0.5,inverse = FALSE)))
}
est_tail11 = inla.posterior.sample.eval(func_tail, samples1)
summary_etatail11 = t(apply(est_tail11,1,quantile,c(0.5, 0.025, 0.975)))

data.frame(true = tail1, summary_etatail11) %>%
  arrange(true) %>%
  ggplot() + geom_point(aes(seq_along(true),true)) +
  geom_line(aes(seq_along(true),X50.)) +
  geom_ribbon(aes(x = seq_along(true), ymin = X2.5., ymax = X97.5.), alpha  = 0.3)
