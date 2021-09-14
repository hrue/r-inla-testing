n = 100

z = rnorm(n)
zz = rnorm(n)
zzz = rnorm(n)
zzzz = rnorm(n)
zzzzz = rnorm(n)
zzzzzz = rnorm(n)

w.z = runif(n)
w.zz = runif(n)
w.zzz = runif(n)
w.zzzz = runif(n)
w.zzzzz = runif(n)
w.zzzzzz = runif(n)

iz = 1:n
izz = 1:n
izzz = 1:n
izzzz = 1:n
izzzzz = 1:n
izzzzzz = 1:n



y = 1 + w.z*z + w.zz*zz + w.zzz* zzz + w.zzzz*zzzz + w.zzzzz*zzzzz + w.zzzzzz*zzzzzz

r = inla(y ~ 1 +
        f(iz, w.z) +
        f(izz, w.zz) +
        f(izzz, w.zzz) +
        f(izzzz, w.zzzz) + 
        f(izzzzz, w.zzzzz) + 
        f(izzzzzz, w.zzzzzz), 
        data = data.frame(
                iz, w.z,
                izz, w.zz,
                izzz, w.zzz,
                izzzz, w.zzzz,
                izzzzz, w.zzzzz,
                izzzzzz, w.zzzzzz), 
        inla.call = "inla.work", num.threads = 1, keep=TRUE, 
        verbose=TRUE)
