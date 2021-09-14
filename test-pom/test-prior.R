library(MCMCpack)

rprior = function(n, nc, alpha) {

    Q = function(x) log(x/(1-x))
    xx = t(apply(rdirichlet(n, rep(alpha, nc)), 1, cumsum))
    xx = Q(xx[, -ncol(xx)])
    return (xx)
}

ns = 10000
nc = 3
alpha = 3
xx = rprior(ns, nc, alpha)

for(k in 1:(nc-1)) {
    d = density(xx[, k])
    if (k == 1) {
        plot(d, xlim = 1.2*range(c(xx)), ylim = c(0, 1.5 * max(d$y)), type="l", lty=k, lwd=2)
    } else {
        lines(d, lty = k, lwd=2)
    }
}

apply(xx, 2,  function(x) print(c(mean=mean(x), sd=sd(x))))

    
    
            
        
