n = 100
idx = rep(1:10, each=10)
y = rnorm(n)
inla.setOption(num.threads=4)
r = inla(y ~ -1 + f(idx, param = c(100, 100)),
         data = data.frame(y, idx), 
         control.inla = list(
             tolerance = 1e-12, 
             h = 0.001, 
             int.strategy = "ccd"), 
         verbose=T)
d = as.matrix(cbind(r$joint.hyper[, 1:2], 1))
rr = inla(y ~ -1 + f(idx, param=c(100, 100)),
          data = data.frame(y, idx), 
          control.mode = list(result = r, restart=FALSE), 
          control.inla = list(
              int.strategy = "user",
              int.design = d,
              h = 0.001, 
              tolerance = 1E-12), 
          verbose=T)
o.r = order(r$joint.hyper[, 1])
r$joint.hyper = r$joint.hyper[o.r, ]
o.rr = order(rr$joint.hyper[, 1])
rr$joint.hyper = rr$joint.hyper[o.rr, ]

r$joint.hyper - rr$joint.hyper
stop("XX")

rrr = inla(y ~ -1 + f(idx, param = c(100, 100)),
           data = data.frame(y), 
           control.inla = list(
               int.strategy = "user",
               int.design = d),
           control.mode = list(result = r, restart=FALSE), 
           verbose=T,
           num.threads = 1)
