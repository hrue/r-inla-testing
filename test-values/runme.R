n = 100
scale = 1:n
idx = 1:n
values = n:1
noise = rnorm(n, sd = 1/sqrt(scale[values]))
z = rnorm(n)
y = 1 + z + noise

lc = inla.make.lincombs(idx = matrix(runif(n^2), n, n))
idx = 1:n
values = n:1
r = inla(y ~ 1 + z + f(idx, model="iid", scale = scale, values = values),
        data = data.frame(y, z, idx, values, scale),
        control.family = list(
                hyper = list(
                        prec = list(
                                initial = 10,
                                fixed = TRUE))),
        verbose = TRUE,
        lincomb = lc, 
        control.compute = list(cpo = TRUE))
summary(r)
