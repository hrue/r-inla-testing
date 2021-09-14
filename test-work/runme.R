inla.my.update()
inla.setOption("inla.call", "inla.work")
n=10

##set.seed(123)
x = rnorm(n)
y = rnorm(n)
z = rnorm(n)

lincomb = inla.make.lincomb(x=1)

r = inla(y ~f(x) + f(z), data = data.frame(x,y,z),
        lincomb = lincomb, num.threads=1)

