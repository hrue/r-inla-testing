vals <- 1:5
data <- list(y = 1:5, x = c(NA, 12:15), idx = c(NA, 2:5), widx = c(1, 0, 3, 4, 5)/10, iidx = 5:1,
             cidx = c(1, 2, 3, 3, 3), vals = vals,
             xx = (1:5)/10)
r <- inla(y ~ 1 + x +
              f(idx, widx, model = "rw1", values = vals, 
                hyper = list(prec = list(initial = 0, fixed = TRUE))) +
              f(iidx, model = "rw1", values = vals,
                hyper = list(prec = list(initial = log(10.0), fixed = TRUE))) +
              f(cidx, copy = "idx") + 
              f(xx, model = "clinear", range = c(1, 2)), 
          control.fixed = list(prec.intercept = 1.1, prec = 10.1),
          data = data, 
          family = "t",
          verbose = T, 
          control.family = list(hyper = list(prec = list(initial = log(10), fixed = TRUE),
                                             dof = list(initial = 6, fixed = TRUE))), 
          inla.call = "inla.mkl.work", inla.arg = "-v -t2:2 -b -P")
r$.args$inla.arg <-  "-v -t2:2 -b"
r$.args$inla.call <-  NULL
rr <- inla.rerun(r)

print(round(dig = 5, cbind(r$mode$x - rr$mode$x)))
print(round(dig = 5, cbind(r$mode$theta - rr$mode$theta)))
