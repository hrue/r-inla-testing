inla.pc.multvar.testing = function(p, lambda=1)
{
    ## test some of the functions
    library(cubature)

    upper = inla.pc.multvar.h.default(1/lambda + 10 * 1/lambda, inverse=TRUE)
    r = (adaptIntegrate(inla.pc.multvar.simplex.d,
                        lowerLimit = rep(0, p),
                        upperLimit = rep(upper, p),
                        lambda = lambda,
                        b = rep(1, p)))
    print(paste("TEST 1: should be 1. result = ", r$integral))

    b = matrix(runif(p), ncol=1)
    b = b/max(b)
    r = (adaptIntegrate(inla.pc.multvar.simplex.d,
                        lowerLimit = rep(0, p),
                        upperLimit = rep(upper, p),
                        lambda = lambda, b=b))
    print(paste("TEST 2: should be 1. result = ", r$integral))

    r = (adaptIntegrate(inla.pc.multvar.sphere.d,
                        lowerLimit = rep(0, p),
                        upperLimit = rep(upper, p),
                        lambda = lambda,
                        H = diag(p)))
    print(paste("TEST 3: should be ", 1, " result = ", 2^p*r$integral))
    
    if (p == 1) {
        H = matrix(1, 1, 1)
    } else {
        H = matrix(0, p, p)
        diag(H) = 1 + runif(p)
        H[lower.tri(H)] = -runif(p^2/2 -p/2)
        H = H %*% t(H)
        diag(H) = 3*diag(H)
        H = H /max(diag(H))
    }
    print(H)
    if (TRUE) {
        library(R2Cuba)
        r = cuhre(p, 1, inla.pc.multvar.sphere.d,
            lower = rep(-upper+0.001, p),
            upper = rep(upper, p),
            lambda = lambda,
            H=H,
            rel.tol =1e-4,
            abs.tol =1e-4, 
            flags = list(verbose=1), 
            max.eval=99999999L,
            key=7L)
        
        print(paste("TEST 4: should be ", 1, " result = ", r$value,
                    " fail = ", r$ifail))
    } else {
        r = (adaptIntegrate(inla.pc.multvar.sphere.d,
                            lowerLimit = rep(-upper+0.001, p),
                            upperLimit = rep(upper, p),
                            lambda = lambda,
                        H = H))
        print(paste("TEST 4: should be ", 1, " result = ", r$integral))
    }
        
}
