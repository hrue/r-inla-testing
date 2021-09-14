source("pc-bym.R")
source("scale.model.R")
inla.mclapply = function(...) INLA:::inla.mclapply(...)

alpha = 0.5
u = 0.5
phi = 1/(1+exp(-seq(-12, 12, len=100000)))
##for(graph in system.file("demodata/germany.graph", package="INLA")) {
##for(graph in system.file("demodata/scotland.graph", package="INLA")) {
for(graph in "small.graph") {
    prior = inla.pc.bym.phi(graph = graph,  u = u,  alpha = alpha, debug=T)
    plot(phi, prior(phi), type="l", lwd=2, ylim = c(-4, 5))
}

Q=inla.pc.bym.Q(graph)
Q = inla.scale.model.bym(Q)
r = inla.bym.constr.internal(Q)
e = eigen(Q)
ev = e$values
e$values[e$values < 1e-8] = 0
e$values[e$values > 0] = 1/e$values[e$values > 0]
v = diag(e$vectors %*% diag(e$values) %*% t(e$vectors))
vv = diag(inla.qinv(Q + Diagonal(nrow(Q))*1e-10, constr=r$constr))
fun = inla.pc.bym.phi(Q, eigenvalues=ev, marginal.variances=v, rankdef = r$rankdef, debug=T,
                      u = u,  alpha = alpha)
lines(phi,  fun(phi), lwd=4, col="blue")


d = read.table("d-matrix.txt",  col.names = c("phi", "d"))
dd = read.table("d-eigen.txt",  col.names = c("phi", "d"))
