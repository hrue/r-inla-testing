graph ="small.graph"
graph = system.file("demodata/scotland.graph", package="INLA")
graph = system.file("demodata/germany.graph", package="INLA")
Q=INLA:::inla.pc.bym.Q(graph)
Q = INLA:::inla.scale.model.bym(Q)
n = nrow(Q)
rankdef = 1
res = INLA:::inla.bym.constr.internal(Q)
e = eigen(Q)
ev = e$values
e$values[e$values < 1e-8] = 0
stopifnot(sum(e$values == 0) == rankdef)
e$values[e$values > 0] = 1/e$values[e$values > 0]
v = diag(e$vectors %*% diag(e$values) %*% t(e$vectors))
f = mean(v)-1
gamma.invm1= c(e$values[1:(n-rankdef)], rep(0, rankdef)) - 1.0
adjust.for.con.comp = TRUE


phi.s = 1/(1+exp(-seq(-14, 0,  len = 10)))
d = numeric(length(phi.s))
k = 1
for(phi in phi.s) {
    aa = n*phi*f
    bb = sum(log1p(phi*gamma.invm1))
    if (aa >= bb) {
        d[k] = sqrt(aa - bb)
    } else {
        d[k] = NA
    }
    k = k + 1
}

eps = 0.01 * .Machine$double.eps


dd = numeric(length(phi.s))
log.q1.det = INLA:::inla.sparse.det.bym(Q, adjust.for.con.comp = adjust.for.con.comp,
                                 constr = res$constr, rankdef = rankdef, eps=eps)

k = 1
for(phi in phi.s) {
    rdef = rankdef 
    aa = n*phi*f
    bb = (n * log((1.0 - phi)/phi) +
          INLA:::inla.sparse.det.bym(Q + phi/(1-phi) * Diagonal(n),  
                              adjust.for.con.comp = adjust.for.con.comp,
                              constr = res$constr, rankdef = rdef, eps=eps) -
          (log.q1.det - n * log(phi)))
    dd[k] = (if (aa >= bb) sqrt(aa-bb) else NA)
    k = k+1
}

print(cbind(phi = phi.s, d.eigen = d,  d.alt = dd,  abs.diff = abs(d - dd)))
    
