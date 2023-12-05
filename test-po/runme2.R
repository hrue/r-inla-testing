n <- 2000
x <- rnorm(n)
log.prec <- -2
y <- x + rnorm(n, sd = sqrt(exp(-log.prec)))
r <- inla(y ~ x,
          data = data.frame(y, x),
          control.family = list(hyper = list(prec = list(initial = log.prec, fixed=TRUE))), 
          control.compute = list(cpo = TRUE, po=TRUE, return.marginals.predictor = TRUE))

val = numeric(n)
for(i in 1:n) {
    val[i] = inla.emarginal(
        function(x, yy) (dnorm(yy, x, sd = sqrt(exp(-log.prec)))), 
        r$marginals.linear.predictor[[i]], yy=y[i])
}
res = cbind(val, r$po$po)
plot(res)
abline(a=0,b=1)

dev.new()
plot(r$cpo$cpo, r$po$po, pch = 19, cex = 0.5)
abline(a=0,b=1)
all(r$cpo$cpo < r$po$po)
