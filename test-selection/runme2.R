n = 10
x = 1+rnorm(n)
xx = 3 + rnorm(n)
y = 1 + x + xx + rnorm(n)
selection = list(xx=1, Predictor = 3:4, x=1)
r = inla(y ~ 1 + x + xx,
         data = data.frame(y, x, xx),
         selection = selection)
ns = 1000
xx = inla.rjmarginal(ns, r)

print(cbind(mean = r$selection$mean, sample.mean = rowMeans(xx$samples)))
print("Cov matrix")
print(round(r$selection$cov.matrix, dig=3))
print("Sample Cov matrix")
print(round(cov(t(xx$samples)), dig=3))

skew = function(x) mean((x-mean(x))^3)/var(x)^1.5
print(round(cbind(skew = r$selection$skewness,
                  sample.skew = apply(xx$sample, 1, skew)), dig=3))

