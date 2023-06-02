n <- 10
y <- rnorm(n)
x <- rnorm(n)
xx <- rnorm(n)
nc <- 2
inla.setOption(num.threads = "1:1")
inla.setOption(smtp = "taucs")

for (m in c("compact", "classic")) {
    inla.setOption(inla.mode = m)
    r <- inla(y ~ 1 + x*xx, 
              data = data.frame(y, idx = 1:n, x, xx),
              control.fixed = list(prec.intercept = 1, prec = 1), 
              family = "t", 
              control.family = list(hyper = list(prec = list(initial = 0, fixed = TRUE),
                                                 dof = list(fixed = TRUE))), 
              control.inla = list(cmin = 0), 
              control.compute = list(internal.opt = FALSE, cpo = TRUE, pit = TRUE), 
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
               control.compute = list(internal.opt = FALSE, cpo = TRUE, pit = TRUE), 
               control.expert = list(disable.gaussian.check = TRUE))
    
}
