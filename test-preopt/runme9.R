n <- 250
y <- (1:n) + rnorm(n)
idx <- inla.group(1:n)

r <- inla(y ~ 1 + f(idx, model = "rw2", scale.model = TRUE),
          data = data.frame(y, idx),
          control.compute = list(cpo = TRUE, po = TRUE, dic = TRUE, waic = TRUE), 
          control.predictor = list(hyper = list(prec = list(initial = 14))), 
          control.inla = list(int.strategy = "grid"), 
          inla.call = "inla.mkl.work",
          inla.arg = "-v -t4 -b",
          verbose = TRUE)
r <- inla.rerun(r)
rr <- inla(y ~ 1 + f(idx, model = "rw2", scale.model = TRUE),
           data = data.frame(y, idx),
           control.compute = list(cpo = TRUE, po = TRUE, dic = TRUE, waic = TRUE), 
           control.predictor = list(hyper = list(prec = list(initial = 14))), 
           control.inla = list(int.strategy = "grid"), 
           inla.call = "inla.mkl.work",
           inla.arg = "-v -t4 -b -P",
           verbose = TRUE, keep = TRUE)
rr <- inla.rerun(rr)

print(mean(abs(r$cpo$cpo - rr$cpo$cpo)))
print(mean(abs(r$po$po - rr$po$po)))

