inla.my.update(b=TRUE)
inla.setOption("num.threads",1)
n = 100
m = 10
N = n*m

z = rnorm(N)
zz = rnorm(m)
zzz = rnorm(m)
y = 1 + z + rep(zz, each = n) + rep(zzz, n)
i = rep(1:m, each = n)
j = rep(1:m, n)
formula = y ~ z + f(i) + f(j)
r = inla(formula, data = data.frame(y, z, i),
        control.inla = list(interpolator = "ccdintegrate"))

rr = inla(formula, data = data.frame(y, z, i),
        control.inla = list(interpolator = "ccd"))

for(j in 1:length(r$marginals.hyperpar)) {
    dev.new()
    plot(r$marginals.hyperpar[[j]])
    lines(rr$marginals.hyperpar[[j]])
}






