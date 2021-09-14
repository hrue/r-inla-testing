n = 1000
n2 = n %/% 2L
link.z = rnorm(n)
link.beta = 1
z = rnorm(n)
eta = 5 + z
y = c(rpois(n2, lambda = exp(eta[1:n2] - link.beta*link.z[1:n2])), 
      rpois(n2, lambda = exp(eta[1:n2 + n2])))
Y = matrix(NA, n, 2)
n2 = n %/% 2L
Y[1:n2, 1] = y[1:n2]
Y[n2 + 1:n2, 2] = y[n2 + 1:n2]

r = inla(Y ~ 1 + z,
        data = list(Y=Y, z=z), 
        family = c(
                "poisson",
                "poisson"), 
        control.family = list(
                list(
                    link = "test1"),
                list(
                    link = "log")),
        control.predictor = list(
                compute=TRUE,
                link = c(rep(1, n2), rep(2, n2))), 
        link.covariates = link.z,
        verbose=TRUE, keep=TRUE)

true.fv = c(
        exp(eta[1:n2] - link.beta * link.z[1:n2]), 
        exp(eta[1:n2+n2]))

plot(true.fv, r$summary.fitted.values$mean)
dev.new()
plot((true.fv-r$summary.fitted.values$mean)/true.fv)

