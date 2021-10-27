n = 100
Q = inla.rw1(n)
i = 1:n
y = (i-n/2)^2 + rnorm(n)

r = inla(y ~ f(i, model="rw1", param = c(1, 1), constr=FALSE) - 1,
         data = data.frame(i,y),
         verbose = T)
rr = inla(y ~ f(i, model="generic", Cmatrix=Q,
        rankdef=1, diagonal = 1e-3, param=c(1, 1), constr=FALSE) - 1,
        data = data.frame(i,y),
        verbose = T)


