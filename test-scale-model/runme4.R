nrow = 25
ncol = 25

a1 = numeric(nrow*ncol)
a2 = numeric(nrow*ncol)
for(i in 1:nrow) {
    for(j in 1:ncol) {
        k = inla.lattice2node(i, j, nrow, ncol)
        a1[k] = i - nrow/2.
        a2[k] = j - ncol/2.
    }
}

cyclic=FALSE
if (cyclic) {
    A = list(A = rbind(rep(1, nrow*ncol)), e=0)
} else {
    A = list(A = rbind(rep(1, nrow*ncol), a1, a2), e = rep(0, 3))
}
              
r1 = inla(y ~ -1 + f(idx, model="rw2d", initial = 0, fixed=TRUE, scale.model=TRUE,
        nrow = nrow, ncol = ncol, constr=FALSE, cyclic = cyclic, 
        extraconstr = A, diagonal = sqrt(.Machine$double.eps)), 
        data = data.frame(y=NA, idx=1), 
        verbose=TRUE,
        control.predictor = list(initial = -10), 
        control.family = list(hyper = list(prec = list(initial = 0, fixed=TRUE))))

print(exp(mean(log(r1$summary.random$idx$sd^2))))
