library(INLA)
library(mvtnorm)
inla.setOption(num.threads = "1:1")
inla.setOption(inla.call = "inla.mkl.work")
INLA:::inla.my.update()

id <- 1:7
n <- length(id)
Q <- solve(toeplitz(0.999^(0:(n-1))))
S <- solve(Q)
y <- as.numeric(rmvnorm(1, mean = rep(1, n), sigma = S)) + (1:n)/n
formula = y~ -1 + f(id, model = "generic", Cmatrix = Q,
                    hyper = list(prec = list(initial = 0, fixed = TRUE)))
r = inla(formula,
         family="gaussian",
         data=list(id=id,y=y),
         inla.mode = "experimental",
         control.family = list(hyper = list(prec = list(initial = 0, fixed = TRUE))), 
         control.compute = list(control.gcpo = list(enable = TRUE,
                                                    group.size = 1,
                                                    verbose = T)), 
         verbose = TRUE)
 
for (g in 1:n) {
    group <- r$gcpo$groups[[g]]$idx
    not.group <- setdiff(1:n, group)
    d <- rep(1, n)
    QQ <- Q + diag(d)
    mm <- solve(QQ, y)
    d[not.group] <- 0
    diag(QQ) <- diag(QQ) - d
    SS <- solve(QQ)
    
    yy <- y
    yy[group] <- 0
    m <- solve(QQ, yy)
    SS <- solve(QQ)

    print(round(dig = 4, c(g = g, gcpo = dnorm(y[g], mean = m[g], sd = sqrt(SS[g, g] + 1)),
                           inla.gcpo = r$gcpo$gcpo[g],
                           mean = m[g], sd = sqrt(SS[g, g]),
                           inla.mean = r$gcpo$mean[g], inla.sd = r$gcpo$sd[g])))
}
