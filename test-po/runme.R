n = 100
y = rnorm(n)
y[2:3] = NA
r = inla(y ~ 1,  data = data.frame(y),
        control.family = list(hyper = list(prec = list(initial = 0, fixed=TRUE))), 
        control.compute = list(po=TRUE))

val = numeric(n)
for(i in 1:n) {
    if (!is.na(y[i])) {
        val[i] = inla.emarginal(function(x, yy) dnorm(yy, x),
                   r$marginals.linear.predictor[[i]], yy=y[i])
    } else {
        val[i] = NA
    }
}
res = cbind(val, r$po$po)
plot(res)
