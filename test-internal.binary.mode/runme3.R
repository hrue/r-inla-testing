inla.my.update()

inla.setOption("inla.call", "inla.work")
inla.setOption("internal.binary.mode",  T)

n = 20000
z = runif(n)
zz = runif(n)
zzz = runif(n)

i = 1:n
y = z + zz +zzz + rnorm(n)
r = inla(y ~ z + zz + zzz,  data = data.frame(z, zz, zzz), keep=T)
