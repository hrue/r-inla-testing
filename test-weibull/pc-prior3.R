kld = function(alpha) 
{
    gam = 0.5772156649
    return ((gamma((1+alpha)/alpha)*alpha + alpha * log(alpha) - alpha*gam + gam -alpha)/alpha)
}
d = function(alpha) 
{
    return (sqrt(2*kld(alpha)))
}

alpha = seq(0.25, 3,  by = 0.0001)
plot(alpha, d(alpha),  type = "l",  bty = "l",  lwd = 3)

alpha = seq(0.125, 3,  by = 0.0001)
dd = d(alpha)
idx = which(alpha >= 1)
dfun1 = splinefun(alpha[idx], dd[idx])
idx = which(alpha < 1)
dfun2 = splinefun(alpha[idx], dd[idx])

lambda = 1
idx = which(alpha < 1)
plot(alpha[idx], lambda/2 * exp(-lambda * dd[idx]) * abs(dfun2(alpha[idx], deriv=1)),
     type = "l",  lwd = 3,  bty = "l", xlim = c(0, max(alpha)))
idx = which(alpha >= 1)
lines(alpha[idx], lambda/2 * exp(-lambda * dd[idx]) * abs(dfun1(alpha[idx], deriv=1)),
      lwd = 3)




    
    
