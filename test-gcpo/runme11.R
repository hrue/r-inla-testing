library(INLA)
inla.setOption(num.threads = "1:1")
inla.setOption(inla.call = "inla.mkl.work")

s <- 0.1
n <- 10
id <- c(1, 2, 2, 3, 3, 3, 4, 4, 4, 4)
y <- rnorm(n, sd = id/10)
formula = y ~ 1 + f(id, model = "iid", initial = 0, fixed = T)
res = inla(formula,
           family="gaussian",
           control.family = list(hyper = list(prec = list(initial = log(1/s^2), fixed = TRUE))), 
           data=data.frame(y, id), 
           inla.mode = "experimental",
           control.compute = list(control.gcpo = list(enable = TRUE, group.size = 2, verbose = !TRUE)),
           control.inla = list(int.strategy = "eb"),
           verbose = T)

res$gcpo$groups
