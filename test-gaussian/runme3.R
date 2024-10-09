N <- 10^5
n <- 1:N
off <- rnorm(N)
y <- off + rnorm(N, sd = sqrt(1/n))
idx <- 1:N

inla.setOption(num.threads = 4)
r <- inla(y ~ 1 + offset(off), 
          scale = n,
##          family = "stdnormal", 
          data = data.frame(y, idx, n, off),
          control.compute = list(dic = T, cpo = T, waic = T, po = T))
summary(r)
rr <- inla(y ~ 1 + offset(off), 
           scale = n,
##           family = "stdnormal", 
           data = data.frame(y, idx, n, off),
           control.compute = list(dic = T, cpo = T, waic = T, po = T),
           inla.call = "inla.mkl.work")
summary(r)

r$dic$dic - rr$dic$dic
r$waic$waic - rr$waic$waic
max(abs(r$cpo$cpo - rr$cpo$cpo))
max(abs(r$po$po - rr$po$po))
r$cpu.intern
rr$cpu.intern
