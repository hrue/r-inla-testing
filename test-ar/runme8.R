n = 10
s1 <- 1.1
s2 <- 1.2
y <- (rnorm(n, sd = s1) + rnorm(n, sd = s2))
idx <- 1:n

inla.setOption(verbose = FALSE, num.threads = 1)
r <- inla(y ~ 1 + f(idx, model='iid'), 
         control.compute = list(internal.opt = FALSE), 
         data = data.frame(y, idx),
         control.expert = list(disable.gaussian.check = TRUE), 
         control.mode = list(theta = log(1/c(s1, s2)^2), fixed = TRUE))

r.retry <- inla(y ~ 1 + f(idx, model='iid'), 
               control.compute = list(internal.opt = FALSE), 
               data = data.frame(y, idx),
               control.mode = list(theta = log(1/c(s1, s2)^2), fixed = TRUE))

rr <- inla(y ~ 1 + f(idx, model='iid'),
          control.compute = list(internal.opt = FALSE), 
          data = data.frame(y, idx),
          inla.call = "inla.mkl.work",
          control.expert = list(disable.gaussian.check = TRUE), 
          control.mode = list(theta = log(1/c(s1, s2)^2), fixed = TRUE))

rr.retry <- inla(y ~ 1 + f(idx, model='iid'),
                control.compute = list(internal.opt = FALSE), 
                data = data.frame(y, idx),
                inla.call = "inla.mkl.work",
                control.mode = list(theta = log(1/c(s1, s2)^2), fixed = TRUE))

r$mlik - r.retry$mlik
rr$mlik - rr.retry$mlik
r$mlik - rr$mlik
r$mlik - rr.retry$mlik
r.retry$mlik - rr.retry$mlik

max(abs(r$mode$x[-(1:n)] - r.retry$mode$x[-(1:n)]))
max(abs(rr$mode$x[-(1:n)] - rr.retry$mode$x[-(1:n)]))
max(abs(r$mode$x[-(1:n)] - rr$mode$x[-(1:n)]))
max(abs(r.retry$mode$x[-(1:n)] - rr.retry$mode$x[-(1:n)]))
max(abs(r$mode$x[-(1:n)] - rr.retry$mode$x[-(1:n)]))
max(abs(r.retry$mode$x[-(1:n)] - rr$mode$x[-(1:n)]))

dif <- c()
for (i in 1:min(length(rr$logfile), length(rr.retry$logfile))) {
    if (rr$logfile[i] != rr.retry$logfile[i]) {
        dif <- c(dif, i, rr$logfile[i], rr.retry$logfile[i])
    }
}


                
                
