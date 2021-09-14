inla.my.update()
inla.setOption("inla.call", "inla.work")

n=5000
m=50

a = runif(n)
b = runif(n)
C = matrix(runif(n*m), n, m)

lc = inla.make.lincombs(a=a, b=b, i=C)

i = 1:n

y = a + b + rnorm(n)
 
r = inla(y ~ a + b + f(i, model="iid"),
        data = data.frame(a,b,i),
        control.family = list(initial=8,fixed=TRUE),
        lincomb = lc,
        verbose = TRUE,
        keep = TRUE)
