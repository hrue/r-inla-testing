n = 200
N = n^2
y = numeric(N)
y = rnorm(N)
i = rep(1:n, rep=n)
j = rep(1:n, each=n)

r = inla(y~-1+f(i, model="rw2") + f(j, model="rw2"),
    data = data.frame(y, i, j), inla.call='inla.small.stack',
    keep=T, 
    verbose=T)
