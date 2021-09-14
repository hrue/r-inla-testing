GAMMA = function(...) gamma(...)
pow = function(x, y) x^y
d1 = function(alpha, lambda, lambda0)
    return (sqrt(2*kld1(alpha, lambda, lambda0)))

kld1 = function(alpha, lambda, lambda0) {
    cg0 = alpha
    cg2 = lambda0
    cg4 = lambda
    return (((cg4 * GAMMA((1 + cg0) / cg0) * cg0) + cg2 * log(cg0) *
             cg0 + cg2 * log(cg2) * cg0 -
             cg2 * cg0 * 0.57721566490153286060651209008240e0 -
             cg2 * cg0 * log(cg4) +
             cg2 * 0.57721566490153286060651209008240e0 -
             cg2 * cg0) / cg2 / cg0) 
}

d2 = function(alpha, lambda, lambda0)
    return (sqrt(2*kld2(alpha, lambda, lambda0)))
kld2 = function(alpha, lambda, lambda0) {
    cg0 = alpha
    cg2 = lambda0
    cg4 = lambda
    return (0.1e1 / cg0 * (cg2 * pow(cg4, -0.1e1 / cg0) *
                           GAMMA((0.1e1 + cg0) / cg0) * cg0 +
                           cg0 * log(cg0) - cg0 * log(cg2) -
                           cg0 * 0.57721566490153286060651209008240e0 +
                           log(cg4) + 0.57721566490153286060651209008240e0 -
                           cg0))
}

q.alpha = 0.5
qs = c(0.5, 1, 1.5, 2)
lty = 1
for(q in qs) {
    alpha = seq(0.5, 2.0, by = 0.01)
    lambda = q / ((-log(1-q.alpha))^(1/alpha))
    lambda0 = q / ((-log(1-q.alpha))^(1/1))
    if (lty == 1) {
        plot(alpha, d1(alpha, lambda, lambda0), ylim = c(0, 2),
             type = "l", lty = lty, main = "lambda defined through quantiles")
    } else {
        lines(alpha, d1(alpha, lambda, lambda0), lty = lty)
    }
    ##lines(alpha, d2(alpha, lambda, lambda0), lty = lty+1)
    lty = lty + 1
}
