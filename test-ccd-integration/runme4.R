inla.my.update(bin=TRUE)

n = 100
i = rep(1:20, each=5)
j = rep(1:5, each=20)
stopifnot(length(i) == n)
z = rnorm(20)
zz = rnorm(5)
y = rnorm(n) + z[i] + z[j]

formula = y ~ 1 + f(i) + f(j)

r = inla(formula, data = data.frame(i,j, y), keep=TRUE, verbose=T, control.inla = list(interpolator = "ccd"))
rr = inla(formula, data = data.frame(i,j, y), keep=TRUE, verbose=T, control.inla = list(interpolator = "ccdintegrate"))


for(j in 1:length(r$internal.marginals.hyperpar)) {
    dev.new()
    plot(rr$internal.marginals.hyperpar[[j]], log="y")
    aa = inla.emarginal(function(x) c(x, x^2), rr$internal.marginals.hyperpar[[j]])
    print(paste("integral", aa[2]-aa[1]^2))
    
    lines(r$internal.marginals.hyperpar[[j]], log="y")

    aa = inla.emarginal(function(x) c(x, x^2), r$internal.marginals.hyperpar[[j]])
    print(paste("approx", aa[2]-aa[1]^2))
    title(paste("hyper", j, "int:dotted, approx:line"))
}
