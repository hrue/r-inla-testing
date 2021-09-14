library(evd)

# ------------------ #
# Auxiliar functions #
# ------------------ #
Gx <- function(x, mu1, sigma1, xi)
  pgev(q = x, loc = mu1, scale = sigma1, shape = xi)

qGx <- function(p, mu1, sigma1, xi)
  qgev(p = p, loc = mu1, scale = sigma1, shape = xi)

gx <- function(x, mu1, sigma1, xi)
  dgev(x = x, loc = mu1, scale = sigma1, shape = xi)

Hx <- function(x, mu2, sigma2)
  pgev(q = x, loc = mu2, scale = sigma2, shape = 0)

hx <- function(x, mu2, sigma2)
  dgev(x = x, loc = mu2, scale = sigma2, shape = 0)

get_Hparam <- function(a, b, qa, qb){
  eval_sigma2 <- (b - a)/log(log(qa)/log(qb))
  eval_mu2 <- b + eval_sigma2 * log(log(1/qb))
  return(list(mu2 = eval_mu2, sigma2 = eval_sigma2))
}

px <- function(x, alpha, beta, a, b){
  y = (x - a)/(b - a)
  return(pbeta(y, shape1 = alpha, shape2 = beta))
}

px.prime <- function(x, alpha, beta, a, b){
  y = (x - a)/(b - a)
  return((1/(b - a))*dbeta(y, shape1 = alpha, shape2 = beta))
}

####################################################
## Our proposed distribution and density function ##
####################################################
Fx <- function(x, mu1, sigma1, xi, alpha, beta, a, b, qa, qb){
  if(alpha > 1 & beta > 1){
    tmp <- get_Hparam(a, b, qa, qb)
    mu2 <- tmp$mu2
    sigma2 <- tmp$sigma2
    eval_p <- px(x, alpha, beta, a, b)
    eval_G <- Gx(x, mu1, sigma1, xi)
    eval_H <- Hx(x, mu2, sigma2)
    return(eval_G^eval_p * eval_H^(1 - eval_p))
  }else
    return(0)
}

fx <- function(x, mu1, sigma1, xi, alpha, beta, a, b, qa, qb){
  if(alpha > 1 & beta > 1){
    tmp <- get_Hparam(a, b, qa, qb)
    mu2 <- tmp$mu2
    sigma2 <- tmp$sigma2
    eval_F <- Fx(x, mu1, sigma1, xi, alpha, beta, a, b, qa, qb)
    eval_p <- px(x, alpha, beta, a, b)
    eval_G <- Gx(x, mu1, sigma1, xi)
    eval_H <- Hx(x, mu2, sigma2)
    eval_p.prime <- px.prime(x, alpha, beta, a, b)
    eval_g <- gx(x, mu2, sigma1, xi)
    eval_h <- hx(x, mu1, sigma2)
    
    return(eval_F * (eval_p.prime * log(eval_G) + eval_p * eval_g/eval_G - eval_p.prime * log(eval_H) + (1 - eval_p) * eval_h/eval_H))
  }else
    return(0)
}

# Testing values ----
mu1 = 0; sigma1 = 1.1; xi = 0.1
qa = 0.02
qb = 0.25
a = qGx(qa, mu1, sigma1, xi)
b = qGx(qb, mu1, sigma1, xi)
alpha = 2
beta = 1.5
x = seq(-5, 15, length = 500)


# Distribution function plot ----
plot(x, sapply(x, Fx, mu1 = mu1, sigma1 = sigma1, xi = xi, alpha = alpha, beta = beta, a = a, b = b, qa = qa, qb = qb), type = 'n', ylab = '', xlab = '', xaxt='n')
lim <- par("usr")
rect(a, lim[3]-1, b, lim[4]+1, border = cols[3], col = cols[3])
lines(x, sapply(x, Fx, mu1 = mu1, sigma1 = sigma1, xi = xi, alpha = alpha, beta = beta, a = a, b = b, qa = qa, qb = qb), lwd = 2)
lines(x, sapply(x, px, alpha = alpha, beta = alpha, a = a, b = b), col = 'purple', lwd = 1.5)
lines(x, sapply(x, Gx, mu1 = mu1, sigma1 = sigma1, xi = xi), col = 'grey60', lwd = 2, lty = 2)
tmp <- get_Hparam(a, b, qa, qb)
mu2 <- tmp$mu2; sigma2 <- tmp$sigma2
lines(x, sapply(x, Hx, mu2 = mu2, sigma2 = sigma2), col = 'lightpink3', lwd = 2, lty = 2)
axis(1, at = c(a), labels = c('a'))
axis(1, at = c(b), labels = c('b'))
legend('bottomright', c('F', 'G', 'H', 'p'), col = c(1, 'grey60', 'lightpink3', 'purple'), lty = c(1, 2, 2, 1))

# Density function plot ----
plot(x, sapply(x, fx, mu1 = mu1, sigma1 = sigma1, xi = xi, alpha = alpha, beta = beta, a = a, b = b, qa = qa, qb = qb), type = 'n', ylab = '', xlab = '', xaxt='n', ylim = c(0, 0.36))
lim <- par("usr")
rect(a, lim[3]-1, b, lim[4]+1, border = cols[3], col = cols[3])
lines(x, sapply(x, fx, mu1 = mu1, sigma1 = sigma1, xi = xi, alpha = alpha, beta = beta, a = a, b = b, qa = qa, qb = qb), lwd = 2)
lines(x, sapply(x, gx, mu1 = mu1, sigma1 = sigma1, xi = xi), col = 'grey60', lwd = 2, lty = 2)
tmp <- get_Hparam(a, b, qa, qb)
mu2 <- tmp$mu2; sigma2 <- tmp$sigma2
lines(x, sapply(x, hx, mu2 = mu2, sigma2 = sigma2), col = 'lightpink3', lwd = 2, lty = 2)
axis(1, at = c(a), labels = c('a'))
axis(1, at = c(b), labels = c('b'))
legend('topright', c('f', 'g', 'h'), col = c(1, 'grey60', 'lightpink3'), lty = c(1, 2, 2))

# log-Density function plot ----
plot(x, log(sapply(x, fx, mu1 = mu1, sigma1 = sigma1, xi = xi, alpha = alpha, beta = beta, a = a, b = b, qa = qa, qb = qb)), type = 'n', ylab = '', xlab = '', xaxt='n')
lim <- par("usr")
rect(a, lim[3]-1, b, lim[4]+1, border = cols[3], col = cols[3])
lines(x, log(sapply(x, fx, mu1 = mu1, sigma1 = sigma1, xi = xi, alpha = alpha, beta = beta, a = a, b = b, qa = qa, qb = qb)), lwd = 2)
lines(x, log(sapply(x, gx, mu1 = mu1, sigma1 = sigma1, xi = xi)), col = 'grey60', lwd = 2, lty = 2)
tmp <- get_Hparam(a, b, qa, qb)
mu2 <- tmp$mu2; sigma2 <- tmp$sigma2
lines(x, log(sapply(x, hx, mu2 = mu2, sigma2 = sigma2)), col = 'lightpink3', lwd = 2, lty = 2)
axis(1, at = c(a), labels = c('a'))
axis(1, at = c(b), labels = c('b'))
legend('bottomright', c('f', 'g', 'h'), col = c(1, 'grey60', 'lightpink3'), lty = c(1, 2, 2))
