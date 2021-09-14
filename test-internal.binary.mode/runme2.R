inla.setOption("inla.call", "inla.work")
inla.setOption("internal.binary.mode", TRUE)

n = 200
Q = toeplitz(c(2,-1, rep(0,n-3),-1))
Q[1,1] = Q[n,n]=1
Q[1,n] = Q[n,1]=0

i = 1:n
y = ((i-n/2)^2)/(0.1*(n/2)^2) + rnorm(n)

r = inla(y ~ f(i, model="rw1",  initial=4, fixed=T) + 1,
        data = data.frame(i,y), verbose=T,
        control.data = list(initial = 0,  fixed=TRUE))

Q = as(Q, "dgTMatrix")
rr = inla(y ~ f(i, model="generic", Cmatrix=Q, rankdef=1, initial=4, fixed=T, constr=T) + 1,
        data = data.frame(i,y), verbose=T, 
        control.data = list(initial = 0,  fixed=TRUE),
        control.inla = list(verbose=T), keep=T)

summary(r$summary.random$i$sd / rr$summary.random$i$sd)

