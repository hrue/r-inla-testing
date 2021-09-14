n = 100
x = 1+rnorm(n)
xx = 3 + rnorm(n)
y = 1 + x + xx + rnorm(n, sd=.1)
selection = list(xx=1, Predictor = 3:4, x=1)
r = inla(y ~ 1 + x + xx,
         data = data.frame(y, x, xx),
         selection = selection)

skew = function(x) mean((x-mean(x))^3)/var(x)^1.5

ns = 1000000
xx = inla.posterior.sample(ns, r, selection)
xx = matrix(unlist(lapply(xx, function(x)x$latent)), ncol = ns)

nm = names(which(inla.posterior.sample.interpret.selection(selection,r)))
rownames(xx) = nm
nnm = r$selection$names
round(rowMeans(xx)[nnm], dig=4)
round(cov(t(xx))[nnm, nnm], dig=4)

round(r$selection$mean, dig=4)
round(r$selection$cov, dig=4)
round(r$selection$skew, dig=4)


