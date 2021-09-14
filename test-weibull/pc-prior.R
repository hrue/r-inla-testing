dwei = function(x, lambda, alpha=1, log=FALSE) {
    xl = x/lambda
    ld = log(alpha/lambda) + (alpha-1) * log(xl) - xl^alpha
    return (if (log) ld else exp(ld))
}

mwei = function(lambda, alpha = 1) {
    return (lambda * gamma(1 + 1/alpha))
}

lambda.from.q.wei = function(q, qlevel, alpha = 1) {
    ## return lambda for a given quantile q, quantile level and alpha
    return (q/((-log(1-qlevel))^(1/alpha)))
}

lambda.from.mean.wei = function(mean.value,  alpha = 1) {
    return (mean.value / gamma(1 + 1/alpha))
}
                                
d1 = function(alpha, q = NULL, mean.value=NULL) {
    kl = kld1(alpha, q, mean.value)
    return (sqrt(2*pmax(0, kl)))
}

kld1 = function(alpha, q=NULL, mean.value=NULL) {
    if (!is.null(q)) {
        q.level = 0.5
        lambda = lambda.from.q.wei(q, q.level, alpha)
        llambda = lambda.from.q.wei(q, q.level)
    } else {
        lambda = lambda.from.mean.wei(mean.value, alpha)
        llambda = lambda.from.mean.wei(mean.value)
    }
    dy = 0.001
    y = exp(seq(-25, 3, by = dy))
    ny = length(y)
    lld = dwei(c(y, y), c(lambda, llambda), rep(c(alpha, 1), each=ny), log=TRUE)
    ld = lld[1:ny]
    ldd = lld[-(1:ny)]
    w = c(y[2]-y[1], y[-(1:2)]-y[-((ny-1):ny)], y[ny]-y[ny-1])/2
    return (sum(exp(ld)*(ld-ldd)*w))
}

alpha = seq(0.5, 2.0, by = 0.01)
n = length(alpha)
d = c()
##D.q = matrix(ncol=0, nrow=n)
first = TRUE
for(q in seq(0.5, 2.0, by = 0.25)) {
    d = c()
    for(a in alpha) {
        d = c(d, d1(alpha=a, q=q))
    }
    ##D.q = cbind(D.q, d)
    if (first) {
        plot(alpha, d, type="l", lwd = 2, ylim = c(0, 1), main='quantile')
        first = FALSE
    } else {
        lines(alpha, d, lwd = 2, lty = 2)
    }
}

dev.new()
alpha = seq(0.5, 2.0, by = 0.01)
n = length(alpha)
##D.m = matrix(ncol=0, nrow=n)
d = c()
first = TRUE
for(mean.value in seq(0.5, 2.0, by = 0.25)) {
    d = c()
    for(a in alpha) {
        d = c(d, d1(alpha=a, mean.value=mean.value))
    }
    ##D.m = cbind(D.m, d)
    if (first) {
        plot(alpha, d, type="l", lwd = 2, ylim = c(0, 1), main = 'mean value')
        first = FALSE
    } else {
        lines(alpha, d, lwd = 2, lty = 2)
    }
}


    
    
