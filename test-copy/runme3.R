inla.setOption(inla.call = "inla.mkl.work")
inla.setOption(inla.mode = "experimental")

n = 1000
N = 4*n
time = 1:n
x = sin(time/n*4*pi)
age = rep(1:4, each=n)

y = numeric(N)
y[ which(age==1) ] = x
y[ which(age==2) ] = 2*x
y[ which(age==3) ] = 3*x
y[ which(age==4) ] = 4*x

y = y + rnorm(N)

formula = y ~ -1 + f(i, model="rw2") +
    f(j, copy="i",  fixed=FALSE) + 
    f(k, copy="i",  fixed=FALSE) + 
    f(l, copy="i",  fixed=FALSE)

i = rep(NA, N)
j = rep(NA, N)
k = rep(NA, N)
l = rep(NA, N)
i[which(age == 1)] = time
j[which(age == 2)] = time
k[which(age == 3)] = time
l[which(age == 4)] = time

r = inla(formula,  data = data.frame(y, i, j, k, l), 
    control.compute = list(dic=TRUE, waic=TRUE))

## swap 'i' and 'l'
formula = y ~ -1 + f(l, model="rw2") +
    f(j, copy="l",  fixed=FALSE) + 
    f(k, copy="l",  fixed=FALSE) + 
    f(i, copy="l",  fixed=FALSE)

rr = inla(formula,  data = data.frame(y, i, j, k, l),
    control.compute = list(dic=TRUE, waic=TRUE))

## the results should differ...
cbind(diff.dic = r$dic$dic-rr$dic$dic, 
      diff.waic = r$waic$waic-rr$waic$waic, 
      diff.ml = r$ml[1]-rr$ml[2])


## make the copy 'nested'
formula = y ~ -1 + f(i, model="rw2") +
    f(j, copy="i",  fixed=FALSE) + 
    f(k, copy="j",  fixed=FALSE) + 
    f(l, copy="k",  fixed=FALSE)

rrr = inla(formula,  data = data.frame(y, i, j, k, l),
    control.compute = list(dic=TRUE, waic=TRUE))

## the results should differ...
cbind(diff.dic = r$dic$dic-rrr$dic$dic, 
      diff.waic = r$waic$waic-rrr$waic$waic, 
      diff.ml = r$ml[1]-rrr$ml[2])


