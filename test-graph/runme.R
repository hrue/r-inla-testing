graphics.off()

n = 1000
formula = y ~ f(jj)  + f(j, model="ar1") + f(jjj) + 1 + z
z =  runif(n)
idx=1:n
j = 1:n
jjj = 1:n
jj = sample(1:20, n, replace=T)
y=rnorm(n)

r = inla(formula, data = data.frame(idx, j, jj, jjj, z),
        control.compute=list(graph=TRUE, q=T),
        verbose=TRUE, debug=F, keep=TRUE, 
        control.inla =
        list(
             global.node.factor = 0.25,
             global.node.degree = 100
             )
        )

if (TRUE) {
    Q = inla.graph2matrix(r$graph)
    re = r$misc$reordering

    diag(Q) = 10*n
    ## dev.new()
    ## inla.spy(Q, re)

    rre = re
    rre[re] = 1:length(re)
    Q.r = Q[rre, rre]
    L.r = t(chol(Q.r))
    print(sum(L.r != 0))
}
