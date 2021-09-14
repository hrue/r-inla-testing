##inla.setOption("inla.call",  "inla.work")
##inla.my.update()
graph.file = system.file("demodata/germany.graph", package="INLA")
g = inla.read.graph(graph.file)
## many samples
nrep = 3
## the weight parameter
phi = 1.2
tau = 1
Q = matrix(0, g$n, g$n)
diag(Q) = tau * (1 + phi * g$nnbs)
for(i in 1:g$n) {
    if (g$nnbs[i] > 0) {
        Q[i, g$nbs[[i]]] = -tau * phi
        Q[g$nbs[[i]], i] = -tau * phi
    }
}
## using dense matrix algebra; ok here...
R = chol(Q)

## make replications
y = c()
for(i in 1:nrep) {
    y = c(y, backsolve(R, rnorm(g$n)))
}

i = rep(1:g$n, nrep)
replicate = rep(1:nrep, each = g$n)

formula = y ~ f(i, model="besagproper",  graph.file = graph.file, replicate=replicate) -1
r = inla(formula, data = data.frame(y, i, replicate), family = "gaussian",
        control.data = list(hyper = list(prec = list(initial = 6,  fixed=TRUE))),
        verbose=TRUE, keep=TRUE)
