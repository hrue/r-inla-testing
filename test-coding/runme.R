n <- 10
y <- rnorm(n)
nc <- 2
A <- matrix(rnorm(n*nc), nc, n)
e <- rnorm(nc)
constr <- list(A = A, e = e)
x <- rnorm(n)
xx <- rnorm(n)
inla.setOption(num.threads = "1:1")
inla.setOption(smtp = "taucs")

for (m in c("compact", "classic")) {
    inla.setOption(inla.mode = m)
    r <- inla(y ~ 1 + x*xx + f(idx, extraconstr = constr, constr = T, 
                               hyper = list(prec = list(initial = 0, fixed = TRUE))), 
              data = data.frame(y, idx = 1:n, x, xx),
              control.fixed = list(prec.intercept = 1, prec = 1), 
              family = "t", 
              control.family = list(hyper = list(prec = list(initial = 0, fixed = TRUE),
                                                 dof = list(fixed = TRUE))), 
              control.inla = list(cmin = 0), 
              control.compute = list(internal.opt = FALSE), 
              control.expert = list(disable.gaussian.check = TRUE))
    rr <- inla(y ~ 1 + x*xx + f(idx, extraconstr = constr, constr = T, 
                                hyper = list(prec = list(initial = 0, fixed = TRUE))), 
               data = data.frame(y, idx = 1:n, x, xx),
               control.fixed = list(prec.intercept = 1, prec = 1), 
               family = "t", 
               control.family = list(hyper = list(prec = list(initial = 0, fixed = TRUE), 
                                                  dof = list(fixed = TRUE))), 
               inla.call = "inla.mkl.work", 
               control.inla = list(cmin = 0), 
               control.compute = list(internal.opt = FALSE), 
               control.expert = list(disable.gaussian.check = TRUE))
    
    print(mean(abs(unlist(c(r$summary.random$idx[, 2:6] - rr$summary.random$idx[, 2:6])))))
}
