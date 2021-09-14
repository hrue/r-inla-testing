lc1 = list(a=list(idx=c(1,2),weight=c(1,2)),
        b=list(idx=1,weight=1),
        c=list(idx=c(2,3),weight=c(2,3)))
lc2 = list(a=list(idx=c(1,2),weight=c(10,20)),
        b=list(idx=1,weight=10),
        c=list(idx=c(2,3),weight=c(20,30)))

lincomb = list(lc1 = lc1, lc2 = lc2)
inla.my.update()

n = 10
a=1:n
b=1:n
c=1:n

y = runif(n) + rnorm(a) + rnorm(b) + rnorm(c)

r = inla(y ~ f(a)+f(b)+f(c)-1,  data = data.frame(a,b,c,y),
        lincomb = lincomb, keep=T)

