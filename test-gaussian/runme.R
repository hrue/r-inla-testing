n = 300
if (FALSE) {
    v = 3.0
    s = sqrt(v)
    x = rnorm(n)
    y = 1 + x + rnorm(n, sd = s)
    r = inla(y ~ x,
             data = data.frame(y, x),
             verbose = TRUE)
    summary(r)
}

v0 = 1.0 ## fixed offset
v1 = 2.0
v = v0 + v1
s = sqrt(v)
x = rnorm(n)
y = 1 + x + rnorm(n, sd = s)
rr = inla(y ~ x,
         data = data.frame(y, x),
         control.family = list(
             hyper = list(precoffset = list(initial = log(1/v0), fixed=1))), 
         verbose = TRUE)
summary(rr)
plot(rr$internal.marginals.hyperpar[[1]], type = "l", lwd=3)
abline(v = log(1.0/v1), lwd=3, col = "blue")


         
