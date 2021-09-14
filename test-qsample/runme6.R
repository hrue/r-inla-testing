library(INLA)
library(digest)

inla.setOption(num.threads = "1:1")
inla.setOption(blas.num.threads = "1")
inla.setOption(smtp = "taucs")

for(a in c(TRUE, FALSE)) {

    inla.setOption('mkl', a)
    cat("mkl=", a, "\n")
    seed <- 123
    set.seed(seed)

    r = inla(y ~ 1, data = data.frame(y = 1:5), 
             control.compute = list(config=TRUE))
    xx <- as.numeric(unlist(r$mode$x))
    xx <- xx[!is.na(xx)]
    print(sapply(xx, sha1, digits = 8))
    
    x = inla.posterior.sample(1, r, selection = list('(Intercept)' = 0), seed = seed)
    x <- as.numeric(unlist(x))
    print(sapply(x, sha1, digits = 8))
}

