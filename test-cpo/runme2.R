## no cpo
n = 50
s = 0.01
x = sort(runif(n))
y = 1 + x + rnorm(n, sd = s)
rr = inla(y~1+x, data = data.frame(x, y),
    family = "gaussian")

## do random 10-fold cpo
idx = sort(sample(1:n, 10,  replace=FALSE))
r = rr
r$.args$control.expert = list(cpo.manual = TRUE,  cpo.idx = idx)
r = inla.rerun(r)
exp(mean(log(r$cpo$cpo[idx])))
